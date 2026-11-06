import { expect, test } from '@playwright/test'

test('updates do not redeal off-screen hands, but still deal new cards', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  const result = await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { dealFrom } = await load('/src/composables/dealing.ts')
    const pile = document.createElement('div')
    pile.dataset.pile = 'deck-other'
    pile.innerHTML = '<div class="stack" style="width:100px;height:140px"></div>'
    document.body.append(pile)
    const view = (ids: string[]) => ({ players: [{ id: 'other', hand: ids.map(id => ({ id, code: null })) }] })
    const before = view(['existing'])
    const unchanged = dealFrom(before, view(['existing']), () => [])
    const repeated = !!unchanged
    unchanged?.lift()
    unchanged?.done()
    const changed = dealFrom(before, view(['existing', 'new']), () => [])
    const ghosts = [...document.querySelectorAll<HTMLElement>('body > img')].map(el => el.style.getPropertyValue('view-transition-name'))
    changed?.lift()
    changed?.done()
    pile.remove()
    return { repeated, ghosts }
  })
  expect(result.repeated).toBe(false)
  expect(result.ghosts).toEqual(['cnew'])
})
