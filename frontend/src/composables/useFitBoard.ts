import { onBeforeUnmount, onMounted, onUpdated, watch, type Ref } from 'vue'
import { tableauWidth } from './useFitToWidth'
import { autoScale } from '../preferences'

/** Zoom the whole board, cards and all around them, so the table fits, reserving enough height for the hero. */
export const useFitBoard = (board: Readonly<Ref<HTMLElement | null>>) => {
  let frame = 0
  const probe = document.createElement('div')
  probe.style.cssText = 'position:fixed;visibility:hidden;pointer-events:none;width:var(--preferred-card-width);height:0'
  const fit = () => {
    const el = board.value
    if (!el) return
    if (matchMedia('(max-width: 760px)').matches || el.classList.contains('waiting')) {
      el.style.removeProperty('--board-zoom')
      el.style.removeProperty('--hand-height')
      return
    }
    const player = el.querySelector<HTMLElement>('.player-half')
    const content = player?.querySelector<HTMLElement>('.half')
    if (!player || !content || !el.clientHeight) return
    if (!autoScale.value) {
      // the board at full size; only the hand's space is still kept
      el.style.removeProperty('--board-zoom')
      el.style.setProperty('--hand-height', `${player.querySelector<HTMLElement>('.floating-hand')?.offsetHeight ?? 0}px`)
      return
    }
    const preferred = parseFloat(getComputedStyle(probe).width)
    if (!Number.isFinite(preferred)) return
    // cards keep their preferred size inside the board, which is zoomed to the fitted width
    el.style.removeProperty('--card-width')
    const fits = (width: number) => {
      el.style.setProperty('--board-zoom', `${width / preferred}`)
      const hand = player.querySelector<HTMLElement>('.floating-hand')
      // the floating hand's space at the foot of the board, at this card size
      el.style.setProperty('--hand-height', `${hand?.offsetHeight ?? 0}px`)
      const tableausFit = [...player.querySelectorAll<HTMLElement>('.player-tableau')].every(tableau => {
        const area = tableau.parentElement!
        const style = getComputedStyle(area)
        const available = area.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight)
        return tableauWidth(tableau) <= available + 1
      })
      return el.scrollHeight <= el.clientHeight + 1
        && tableausFit
        && content.scrollHeight <= content.clientHeight + 1
        && (hand?.offsetHeight ?? 0) <= player.clientHeight + 1
        && [...el.querySelectorAll<HTMLElement>('.villain-area, .river')]
          .every(zone => zone.scrollWidth <= zone.clientWidth + 1)
    }
    // Wrapping makes height discontinuous. Search actual layout rather than
    // estimating a ratio from the current number of rows.
    let low = Math.min(40, preferred)
    let high = preferred
    if (fits(preferred)) low = preferred
    else {
      for (let n = 0; n < 8; n++) {
        const candidate = (low + high) / 2
        if (fits(candidate)) low = candidate
        else high = candidate
      }
    }
    fits(Math.floor(low * 10) / 10)
  }
  const schedule = () => {
    cancelAnimationFrame(frame)
    frame = requestAnimationFrame(fit)
  }
  const resize = new ResizeObserver(schedule)
  let observed = new Set<Element>()
  const observe = () => {
    const elements = new Set<Element>(board.value
      ? [board.value, ...board.value.querySelectorAll('.player-area, .player-tableau, .villain-area, .river')]
      : [])
    for (const element of observed) if (!elements.has(element)) resize.unobserve(element)
    for (const element of elements) if (!observed.has(element)) resize.observe(element)
    observed = elements
  }
  watch(autoScale, schedule)
  onMounted(() => {
    document.body.append(probe)
    observe()
    fit()
    document.fonts.addEventListener('loadingdone', schedule)
  })
  // Run before transition snapshots are captured when game contents change.
  onUpdated(() => { observe(); fit() })
  onBeforeUnmount(() => {
    document.fonts.removeEventListener('loadingdone', schedule)
    resize.disconnect()
    cancelAnimationFrame(frame)
    probe.remove()
  })
}
