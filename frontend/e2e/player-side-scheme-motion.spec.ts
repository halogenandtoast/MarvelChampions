import { expect, test } from '@playwright/test'

for (const source of ['deck', 'discard', 'encounter', 'set-aside'] as const) {
  test(`a side scheme enters from its ${source} pile`, async ({ page }) => {
    await page.goto('/#/cards/40018')
    const result = await page.evaluate(async source => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { dealFrom, morphName } = await load('/src/composables/dealing.ts')
      const { BACKS, useCards } = await load('/src/cards.ts')
      await useCards().store.load()
      const host = document.createElement('div')
      document.body.replaceChildren(host)
      for (const [pile, left] of [['deck-owner', 50], ['deck-encounter', 350], ['discard-owner', 650], ['set-aside', 950]] as const) {
        const element = document.createElement('div')
        element.dataset.pile = pile
        element.innerHTML = '<div class="stack"></div>'
        element.querySelector<HTMLElement>('.stack')!.style.cssText = `position: fixed; left: ${left}px; top: 100px; width: 100px; height: 140px`
        host.append(element)
      }
      const card = { id: 'selected-scheme', code: source === 'encounter' ? '01109' : '40018', name: 'Side scheme' }
      const before = { players: [{ id: 'owner', hand: [], discard: source === 'discard' ? [card] : [] }], sideSchemes: [], setAsideCards: source === 'set-aside' ? [card] : [] }
      const after = { ...before, setAsideCards: [], players: [{ id: 'owner', hand: [], discard: [] }], sideSchemes: [{ ...card, owner: source === 'encounter' ? null : 'owner' }] }
      const move = dealFrom(before, after, () => ['/card-image.png'])
      const ghost = [...document.body.children].find(el => (el as HTMLElement).style.viewTransitionName === morphName(card.id)) as HTMLElement | undefined
      if (!ghost) throw new Error('No source ghost for the selected side scheme')
      const image = ghost instanceof HTMLImageElement ? ghost : ghost.querySelector('img')!
      const result = { left: ghost.getBoundingClientRect().left, image: image.getAttribute('src'), back: BACKS[source === 'encounter' ? 'encounter' : 'player'] }
      move.done()
      return result
    }, source)
    expect(result.left).toBe(source === 'deck' ? 50 : source === 'discard' ? 650 : source === 'set-aside' ? 950 : 350)
    if (source === 'deck' || source === 'encounter') expect(result.image).toBe(result.back)
    else expect(result.image).toBe('/card-image.png')
  })
}

for (const owner of ['player', 'encounter'] as const) {
  test(`an inspected ${owner} card animates from the matching deck`, async ({ page }) => {
    await page.goto('/#/cards/18001b')
    const result = await page.evaluate(async owner => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { dealFrom, morphName } = await load('/src/composables/dealing.ts')
      const { BACKS, useCards } = await load('/src/cards.ts')
      await useCards().store.load()
      document.body.replaceChildren()
      for (const [pile, left] of [['deck-player', 50], ['deck-encounter', 350]] as const) {
        const element = document.createElement('div')
        element.dataset.pile = pile
        element.innerHTML = `<div class="stack" style="position:fixed;left:${left}px;top:100px;width:100px;height:140px"></div>`
        document.body.append(element)
      }
      const card = { id: 'inspected-top', code: owner === 'player' ? '18004' : '01109', name: 'Inspected card', owner: owner === 'player' ? 'player' : null }
      const before = { players: [{ id: 'player', hand: [], discard: [] }], focusedCards: [] }
      const after = { ...before, focusedCards: [card] }
      const move = dealFrom(before, after, () => ['/card-image.png'])
      const ghost = [...document.body.children].find(element => (element as HTMLElement).style.viewTransitionName === morphName(card.id)) as HTMLImageElement | undefined
      if (!ghost) throw new Error('No inspected-card source ghost')
      const result = { left: ghost.getBoundingClientRect().left, back: ghost.src, expected: new URL(BACKS[owner], location.origin).href }
      move.done()
      return result
    }, owner)
    expect(result.left).toBe(owner === 'player' ? 50 : 350)
    expect(result.back).toBe(result.expected)
  })
}
