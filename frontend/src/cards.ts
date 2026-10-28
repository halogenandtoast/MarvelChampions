// Card codes as the engine names them, to what the board draws: the scan, its MarvelCDB
// fallback, which way up it is printed.
import { useCardStore } from '@browser/stores/cards'
import { CARD_IMAGES, localImage, remoteImage, type Card } from '@browser/types/card'

export const BACKS = {
  player: `${CARD_IMAGES}/back-player.avif`,
  encounter: `${CARD_IMAGES}/back-encounter.avif`,
  villain: `${CARD_IMAGES}/back-villain.avif`,
} as const
export type Back = keyof typeof BACKS

/** A main scheme is set up from its A side and played on its B side, which the engine
 * names by the bare code: show that side. */
export const faceCode = (code: string, cardByCode: (code: string) => Card | undefined): string => {
  const card = cardByCode(code) ?? cardByCode(`${code}a`)
  if (card?.type === 'main_scheme' && !/[a-z]$/.test(code) && cardByCode(`${code}b`)) return `${code}b`
  if (!cardByCode(code) && cardByCode(`${code}a`)) return `${code}a`
  return code
}

/** The images to try for a card, best first; a placeholder follows when all fail. */
export const imageSources = (code: string, cardByCode: (code: string) => Card | undefined): string[] => {
  const card = cardByCode(faceCode(code, cardByCode))
  if (!card) return [`${CARD_IMAGES}/${code}.avif`]
  return [localImage(card), remoteImage(card)].filter((s): s is string => s !== null)
}

export const useCards = () => {
  const store = useCardStore()
  void store.load()
  const card = (code: string | null | undefined) => (code ? store.cardByCode(faceCode(code, store.cardByCode)) : undefined)
  return {
    store,
    card,
    sources: (code: string) => imageSources(code, store.cardByCode),
    landscape: (code: string | null | undefined) => card(code)?.orientation === 'landscape',
    name: (code: string | null | undefined, fallback = '') => card(code)?.name ?? fallback,
  }
}
