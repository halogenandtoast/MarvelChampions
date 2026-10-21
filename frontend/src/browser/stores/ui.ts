/**
 * Presentation preferences that outlive a page load.
 */
import { defineStore } from './store'
import { useStorage } from '@vueuse/core'

export type ViewMode = 'list' | 'grid'

export const useUiStore = defineStore('ui', () => {
  const viewMode = useStorage<ViewMode>('marvel:view', 'grid')
  // shut at first on a screen too narrow for it beside the results (see BrowseView)
  const filtersOpen = useStorage('marvel:filters', !matchMedia('(width < 64rem)').matches)
  return { viewMode, filtersOpen }
})
