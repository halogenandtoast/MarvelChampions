import { nextTick } from 'vue'

type StartViewTransition = (callback: () => void | Promise<void>) => {
  finished: Promise<void>
  updateCallbackDone: Promise<void>
}

const supported = (): StartViewTransition | null => {
  const start = (document as Document & { startViewTransition?: StartViewTransition }).startViewTransition
  return typeof start === 'function' ? start.bind(document) : null
}

/**
 * Applies a state change inside a view transition so cards morph between zones, falling
 * back to a plain update where the API is unavailable.
 */
/** Whether state changes will animate through a view transition. */
export const transitionsOn = () =>
  supported() !== null && !window.matchMedia('(prefers-reduced-motion: reduce)').matches

export const withViewTransition = async (apply: () => void | Promise<void>): Promise<void> => {
  const start = supported()
  if (!start || !transitionsOn()) {
    await apply()
    await nextTick()
    return
  }
  const transition = start(async () => {
    await apply()
    await nextTick()
  })
  await transition.finished.catch(() => undefined)
}
