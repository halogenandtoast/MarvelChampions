import { useStorage } from '@vueuse/core'

/** Shrink the board's cards so the whole table fits the screen; off, cards keep their size and the board scrolls. */
export const autoScale = useStorage('marvel.autoScale', true)
