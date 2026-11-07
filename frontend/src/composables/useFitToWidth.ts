import { ref, type Ref } from 'vue'
import { useResizeObserver } from '@vueuse/core'

/** Fit a tableau without squeezing its individual card groups into narrow columns. */
export const useFitToWidth = (container: Readonly<Ref<HTMLElement | null>>, content: Readonly<Ref<HTMLElement | null>>) => {
  const scale = ref(1)
  useResizeObserver([container, content], () => {
    if (!container.value || !content.value) return
    const style = getComputedStyle(container.value)
    const available = container.value.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight)
    const width = content.value.scrollWidth
    if (available > 0 && width > 0) {
      const fitted = Math.min(1, Math.max(0.45, available / width))
      // Round down so fractional layout pixels cannot alternate across the fit boundary.
      scale.value = Math.floor(fitted * 1000) / 1000
    }
  })
  return scale
}
