import { computed, reactive, readonly, ref, shallowRef } from 'vue'
import { useMediaQuery } from '@vueuse/core'
import type { Back } from '../cards'

/** What the overlay draws: a card by its code, or a back. */
export interface Preview {
  code: string | null
  name?: string
  back?: Back | null
}

const OVERLAY_WIDTH = 320
const CARD_RATIO = 0.716
const GAP = 10
const PAD = 10

interface Placement {
  top: number
  left: number
  width: number
  height: number
}

const card = shallowRef<Preview | null>(null)
const sideways = ref(false)
const anchor = shallowRef<HTMLElement | null>(null)
// shown by a touch rather than a hover: centred, and up until the next touch
const pinned = ref(false)
const placement = reactive<Placement>({ top: 0, left: 0, width: OVERLAY_WIDTH, height: 0 })

let frame: number | null = null
let listening = false
// A touch that opens or closes the preview is only for that, so its click is swallowed.
let pressing = false
let swallowClick = false

const measure = () => {
  const element = anchor.value
  if (!element) return

  const rect = element.getBoundingClientRect()
  const width = sideways.value ? OVERLAY_WIDTH / CARD_RATIO : OVERLAY_WIDTH
  const height = sideways.value ? OVERLAY_WIDTH : Math.round(OVERLAY_WIDTH / CARD_RATIO)

  if (pinned.value) {
    // as big as fits, up to the hover size, in the middle of the screen
    const scale = Math.min(1, (window.innerWidth - 2 * PAD) / width, (window.innerHeight - 2 * PAD) / height)
    placement.width = width * scale
    placement.height = height * scale
    placement.left = (window.innerWidth - placement.width) / 2
    placement.top = (window.innerHeight - placement.height) / 2
    return
  }

  // Prefer the right of the card, flipping to its left when that would run off screen.
  const toTheRight = rect.right + GAP
  const left =
    toTheRight + width + PAD >= window.innerWidth ? rect.left - width - GAP : toTheRight

  // Line up with the card, then lift it so the whole card stays inside the viewport.
  const desiredTop = rect.top - 40
  const maxTop = window.innerHeight - height - PAD
  const top = Math.max(PAD, Math.min(desiredTop, maxTop))

  placement.top = top
  placement.left = Math.max(PAD, Math.min(left, window.innerWidth - width - PAD))
  placement.width = width
  placement.height = height
}

const schedule = () => {
  if (frame !== null) cancelAnimationFrame(frame)
  frame = requestAnimationFrame(() => {
    frame = null
    measure()
  })
}

const hide = () => {
  card.value = null
  anchor.value = null
  pinned.value = false
}

const listen = () => {
  if (listening) return
  listening = true
  window.addEventListener('scroll', schedule, true)
  window.addEventListener('resize', schedule)
  document.addEventListener('pointerdown', () => {
    pressing = true
    swallowClick = pinned.value
    if (pinned.value) hide()
  }, true)
  const release = () => { pressing = false }
  document.addEventListener('pointerup', release, true)
  document.addEventListener('pointercancel', release, true)
  document.addEventListener('click', (event) => {
    if (!swallowClick) return
    swallowClick = false
    event.stopPropagation()
    event.preventDefault()
  }, true)
}

/** A mouse or trackpad to hover with; without one, cards are previewed by touch. */
const canHover = useMediaQuery('(hover: hover)')

export const useCardOverlay = () => (listen(), {
  card: readonly(card),
  sideways: readonly(sideways),
  placement,
  visible: computed(() => card.value !== null),
  canHover,
  show: (shown: Preview, element: HTMLElement, rotated = false, touched = false) => {
    card.value = shown
    sideways.value = rotated
    anchor.value = element
    pinned.value = touched
    swallowClick = touched && pressing
    measure()
  },
  hide,
})
