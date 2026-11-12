import { onBeforeUnmount, onMounted, onUpdated, type Ref } from 'vue'

/** Keep one card size across the board, reserving enough height for the hero. */
export const useFitBoard = (board: Readonly<Ref<HTMLElement | null>>) => {
  let frame = 0
  const probe = document.createElement('div')
  probe.style.cssText = 'position:fixed;visibility:hidden;pointer-events:none;width:var(--preferred-card-width);height:0'
  const fit = () => {
    const el = board.value
    if (!el) return
    if (matchMedia('(max-width: 760px)').matches || el.classList.contains('waiting')) {
      el.style.removeProperty('--card-width')
      return
    }
    const player = el.querySelector<HTMLElement>('.player-half')
    const content = player?.querySelector<HTMLElement>('.half')
    if (!player || !content || !el.clientHeight) return
    const preferred = parseFloat(getComputedStyle(probe).width)
    if (!Number.isFinite(preferred)) return
    const fits = (width: number) => {
      el.style.setProperty('--card-width', `${width}px`)
      const hand = player.querySelector<HTMLElement>('.floating-hand')
      const tableausFit = [...player.querySelectorAll<HTMLElement>('.player-tableau')].every(tableau => {
        const area = tableau.parentElement!
        const style = getComputedStyle(area)
        const available = area.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight)
        const groups = [...tableau.children] as HTMLElement[]
        const gap = parseFloat(getComputedStyle(tableau).columnGap) || 0
        const natural = groups.reduce((total, group) => total + group.offsetWidth, 0) + gap * Math.max(0, groups.length - 1)
        return natural <= available + 1
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
