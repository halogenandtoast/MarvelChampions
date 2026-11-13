import { expect, test } from '@playwright/test'

// An encounter card attached in a player's area (Frozen in Time on the identity, an
// obligation) flies to the encounter discard pile when it is discarded.
for (const zone of ['attachments', 'obligations'] as const) {
  test(`a player's ${zone} card discarded flies to the encounter discard`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    const result = await page.evaluate(async (zone) => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { dealFrom } = await load('/src/composables/dealing.ts')
      const card = { id: 'frozen-in-time', code: '11016', name: 'Frozen in Time' }
      const discard = document.createElement('div')
      discard.dataset.pile = 'discard-encounter'
      discard.innerHTML = '<div class="stack" style="width: 70px; height: 98px"></div>'
      const onTable = document.createElement('div')
      onTable.dataset.cardId = card.id
      document.body.replaceChildren(discard, onTable)
      const before = { players: [{ id: 'groot', [zone]: [card] }], encounterDiscard: [] }
      const after = { players: [{ id: 'groot', [zone]: [] }], encounterDiscard: [card] }
      const move = dealFrom(before, after, () => [])
      if (!move) return { moved: false, landed: 0 }
      move.lift()
      onTable.remove()
      move.land()
      const landed = [...document.querySelectorAll<HTMLElement>('body > img')]
        .filter((ghost) => ghost.style.viewTransitionName === 'cfrozen-in-time').length
      move.done()
      return { moved: true, landed }
    }, zone)
    expect(result).toEqual({ moved: true, landed: 1 })
  })
}
