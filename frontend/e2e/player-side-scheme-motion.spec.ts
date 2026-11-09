import { expect, test } from '@playwright/test'

for (const source of ['deck', 'discard', 'encounter'] as const) {
  test(`a side scheme enters from its ${source} pile`, async ({ page }) => {
    await page.goto('/#/cards/40018')
    const result = await page.evaluate(async source => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { dealFrom, morphName } = await load('/src/composables/dealing.ts')
      const { BACKS, useCards } = await load('/src/cards.ts')
      await useCards().store.load()
      const host = document.createElement('div')
      document.body.replaceChildren(host)
      for (const [pile, left] of [['deck-owner', 50], ['deck-encounter', 350], ['discard-owner', 650]] as const) {
        const element = document.createElement('div')
        element.dataset.pile = pile
        element.innerHTML = '<div class="stack"></div>'
        element.querySelector<HTMLElement>('.stack')!.style.cssText = `position: fixed; left: ${left}px; top: 100px; width: 100px; height: 140px`
        host.append(element)
      }
      const card = { id: 'selected-scheme', code: source === 'encounter' ? '01109' : '40018', name: 'Side scheme' }
      const before = { players: [{ id: 'owner', hand: [], discard: source === 'discard' ? [card] : [] }], sideSchemes: [] }
      const after = { ...before, players: [{ id: 'owner', hand: [], discard: [] }], sideSchemes: [{ ...card, owner: source === 'encounter' ? null : 'owner' }] }
      const move = dealFrom(before, after, () => ['/card-image.png'])
      const ghost = [...document.body.children].find(el => (el as HTMLElement).style.viewTransitionName === morphName(card.id)) as HTMLElement | undefined
      if (!ghost) throw new Error('No source ghost for the selected side scheme')
      const image = ghost instanceof HTMLImageElement ? ghost : ghost.querySelector('img')!
      const result = { left: ghost.getBoundingClientRect().left, image: image.getAttribute('src'), back: BACKS[source === 'encounter' ? 'encounter' : 'player'] }
      move.done()
      return result
    }, source)
    expect(result.left).toBe(source === 'deck' ? 50 : source === 'discard' ? 650 : 350)
    if (source !== 'discard') expect(result.image).toBe(result.back)
  })
}
