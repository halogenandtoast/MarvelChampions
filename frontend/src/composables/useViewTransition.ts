import { nextTick } from 'vue'

type StartViewTransition = (callback: () => void | Promise<void>) => {
  finished: Promise<void>
  updateCallbackDone: Promise<void>
  skipTransition: () => void
}
let revision = 0
const active = new Set<ReturnType<StartViewTransition>>()

export const cancelViewTransitions = () => {
  revision++
  for (const transition of active) transition.skipTransition()
  for (const animation of document.getAnimations()) animation.cancel()
  window.dispatchEvent(new Event('marvel:cancel-animations'))
}

/** Drop captured modal images without cancelling queued board updates or shuffles. */
export const skipViewTransitions = () => {
  for (const transition of active) transition.skipTransition()
}

const supported = (): StartViewTransition | null => {
  const start = (document as Document & { startViewTransition?: StartViewTransition }).startViewTransition
  return typeof start === 'function' ? start.bind(document) : null
}

/**
 * Applies a state change inside a view transition so cards morph between zones, falling
 * back to a plain update where the API is unavailable.
 */
/** Native modals sit below view-transition snapshots. Keep board updates plain
 * while a picker is open, so its dimmer and cards cannot fade or show through. */
export const transitionsOn = () =>
  supported() !== null && !window.matchMedia('(prefers-reduced-motion: reduce)').matches
    && !document.querySelector('dialog:modal, .pick-backdrop')

export const withViewTransition = async (apply: () => void | Promise<void>): Promise<void> => {
  const startedAt = revision
  const start = supported()
  if (!start || !transitionsOn()) {
    await apply()
    await nextTick()
    return
  }
  const transition = start(async () => {
    if (startedAt !== revision) return
    await apply()
    await nextTick()
  })
  active.add(transition)
  try { await transition.finished.catch(() => undefined) }
  finally { active.delete(transition) }
}
