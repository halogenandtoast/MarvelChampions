import { computed, reactive, readonly, ref, shallowRef } from 'vue'
import { useMediaQuery } from '@vueuse/core'
import { useCards, type Back } from '../cards'

/** What the overlay draws: a card by its code, or a back, with the status cards on it. */
export interface Preview {
  code: string | null
  name?: string
  back?: Back | null
  /** stunned, confused, tough: shown beside the card */
  statuses?: readonly string[]
  inactiveStatuses?: readonly string[]
}

const OVERLAY_WIDTH = 320
const CARD_RATIO = 0.716
const GAP = 10
const PAD = 10
// a status card beside the preview, printed landscape
export const STATUS_WIDTH = 200

interface Placement {
  top: number
  left: number
  width: number
  height: number
}

const card = shallowRef<Preview | null>(null)
const sideways = ref(false)
const aspectRatio = ref(CARD_RATIO)
// which side of the preview its status cards go: away from the card hovered
const side = ref<'left' | 'right'>('right')
const anchor = shallowRef<HTMLElement | null>(null)
// shown by a touch rather than a hover: centred, and up until the next touch
const pinned = ref(false)
const placement = reactive<Placement>({ top: 0, left: 0, width: OVERLAY_WIDTH, height: 0 })

let frame: number | null = null
let listening = false
let anchorObserver: MutationObserver | null = null
// A touch that opens or closes the preview is only for that, so its click is swallowed.
let pressing = false
let swallowClick = false

const measure = () => {
  const element = anchor.value
  if (!element) return

  const rect = element.getBoundingClientRect()
  if (!element.isConnected || !rect.width || !rect.height) {
    hide()
    return
  }
  const width = sideways.value ? OVERLAY_WIDTH / CARD_RATIO : OVERLAY_WIDTH
  const height = width / aspectRatio.value
  const extra = card.value?.statuses?.length ? STATUS_WIDTH + GAP : 0

  if (pinned.value) {
    // as big as fits, up to the hover size, in the middle of the screen, statuses and all
    const scale = Math.min(1, (window.innerWidth - 2 * PAD) / (width + extra), (window.innerHeight - 2 * PAD) / height)
    side.value = 'right'
    placement.width = width * scale
    placement.height = height * scale
    placement.left = (window.innerWidth - placement.width - extra * scale) / 2
    placement.top = (window.innerHeight - placement.height) / 2
    return
  }

  // Prefer the right of the card, flipping to its left when that would run off screen; the
  // status cards go on the far side, so they need room too.
  const toTheRight = rect.right + GAP
  const right = toTheRight + width + extra + PAD < window.innerWidth
  side.value = right ? 'right' : 'left'
  const left = right ? toTheRight : Math.max(PAD + extra, rect.left - width - GAP)

  // Line up with the card, then lift it so the whole card stays inside the viewport.
  const desiredTop = rect.top - 40
  const maxTop = window.innerHeight - height - PAD
  const top = Math.max(PAD, Math.min(desiredTop, maxTop))

  placement.top = top
  placement.left = Math.max(PAD, Math.min(left, window.innerWidth - width - PAD - (right ? extra : 0)))
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
  anchorObserver?.disconnect()
  if (frame !== null) cancelAnimationFrame(frame)
  frame = null
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
  side: readonly(side),
  placement,
  // Native modal dialogs live above the document regardless of z-index.
  teleportTarget: computed(() => anchor.value?.closest('dialog[open]') ?? 'body'),
  visible: computed(() => card.value !== null),
  canHover,
  show: (shown: Preview, element: HTMLElement, rotated?: boolean, touched = false) => {
    const landscape = rotated ?? (!shown.back && useCards().landscape(shown.code))
    card.value = shown
    sideways.value = landscape
    aspectRatio.value = (!shown.back && useCards().card(shown.code)?.aspectRatio) || (landscape ? 1 / CARD_RATIO : CARD_RATIO)
    anchor.value = element
    pinned.value = touched
    swallowClick = touched && pressing
    measure()
    // Removing a hovered card does not reliably fire pointerleave, especially during
    // a view transition. Stop previewing it as soon as its anchor leaves the board.
    anchorObserver ??= new MutationObserver(() => {
      if (anchor.value && !anchor.value.isConnected) hide()
    })
    anchorObserver.disconnect()
    if (anchor.value) anchorObserver.observe(document.body, { childList: true, subtree: true })
  },
  hide,
})
