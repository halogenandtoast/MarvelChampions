import { ref, type Ref } from 'vue'
import { useMutationObserver, useResizeObserver } from '@vueuse/core'

/** Fit a tableau without squeezing its individual card groups into narrow columns. */
export const useFitToWidth = (container: Readonly<Ref<HTMLElement | null>>, content: Readonly<Ref<HTMLElement | null>>) => {
  const scale = ref(1)
  const width = ref<number | undefined>()
  const fit = () => {
    if (!container.value || !content.value) return
    const style = getComputedStyle(container.value)
    const available = container.value.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight)
    const groups = [...content.value.children] as HTMLElement[]
    const gap = parseFloat(getComputedStyle(content.value).columnGap) || 0
    // Ignore distributed free space: only the groups' natural widths determine scale.
    const naturalWidth = groups.reduce((total, group) => total + group.offsetWidth, 0) + gap * Math.max(0, groups.length - 1)
    if (available > 0 && naturalWidth > 0) {
      const fitted = Math.min(1, Math.max(0.45, available / naturalWidth))
      // Round down so fractional layout pixels cannot alternate across the fit boundary.
      scale.value = Math.floor(fitted * 1000) / 1000
      width.value = available / scale.value
    }
  }
  useResizeObserver([container, content], fit)
  useResizeObserver(() => [...(content.value?.children ?? [])] as HTMLElement[], fit)
  useMutationObserver(content, fit, { childList: true, subtree: true, attributes: true, attributeFilter: ['class'] })
  return { scale, width }
}
