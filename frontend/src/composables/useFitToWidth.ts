import { onMounted, onUpdated, ref, watch, type Ref } from 'vue'
import { useMutationObserver, useResizeObserver } from '@vueuse/core'
import { autoScale } from '../preferences'

// Ignore distributed free space: only the groups' natural widths count. A group of
// rows stacked one over another (marked data-fit-rows) is as wide as its widest row.
const groupWidth = (group: HTMLElement): number =>
  group.dataset.fitRows !== undefined
    ? Math.max(0, ...([...group.children] as HTMLElement[]).map(tableauWidth))
    : group.offsetWidth

/** A tableau's natural width: its groups side by side. */
export const tableauWidth = (row: HTMLElement): number => {
  const groups = [...row.children] as HTMLElement[]
  const gap = parseFloat(getComputedStyle(row).columnGap) || 0
  return groups.reduce((total, group) => total + groupWidth(group), 0) + gap * Math.max(0, groups.length - 1)
}

/** Fit a tableau without squeezing its individual card groups into narrow columns. */
export const useFitToWidth = (container: Readonly<Ref<HTMLElement | null>>, content: Readonly<Ref<HTMLElement | null>>) => {
  const scale = ref(1)
  const width = ref<number | undefined>()
  const fit = () => {
    if (!container.value || !content.value) return
    if (!autoScale.value || matchMedia('(max-width: 760px)').matches) {
      scale.value = 1
      width.value = undefined
      return
    }
    const style = getComputedStyle(container.value)
    const available = container.value.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight)
    const naturalWidth = tableauWidth(content.value)
    if (available > 0 && naturalWidth > 0) {
      const fitted = container.value.closest('[data-fit-board]')
        ? 1
        : Math.min(1, Math.max(0.45, available / naturalWidth))
      // Round down so fractional layout pixels cannot alternate across the fit boundary.
      scale.value = Math.floor(fitted * 1000) / 1000
      width.value = available / scale.value
    }
  }
  // Fit in Vue's render cycle, before a view transition captures the new board.
  // ResizeObserver alone runs after that snapshot and briefly exposes full-size cards.
  onMounted(fit)
  watch(autoScale, fit)
  onUpdated(fit)
  useResizeObserver([container, content], fit)
  useResizeObserver(() => [...(content.value?.children ?? [])].flatMap((child) =>
    (child as HTMLElement).dataset.fitRows !== undefined
      ? [child, ...[...child.children].flatMap((row) => [row, ...row.children])]
      : [child]) as HTMLElement[], fit)
  useMutationObserver(content, fit, { childList: true, subtree: true, attributes: true, attributeFilter: ['class'] })
  return { scale, width }
}
