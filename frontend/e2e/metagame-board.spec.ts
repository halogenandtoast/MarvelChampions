import { expect, test } from '@playwright/test'

for (const spaces of [6, 9]) {
  test(`${spaces}-space metagame board shows saved tokens and selects an affordable space once`, async ({ page }) => {
    await page.goto('/#/cards/44057')
    await page.evaluate(async (spaces) => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h, reactive } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: BoardCard } = await load('/src/components/BoardCard.vue')
      // Reuse the component's module URL, including Vite's HMR stamp, so its
      // injection key is the same object as the provider's.
      const componentSource = await (await fetch('/src/components/BoardCard.vue')).text()
      const choicesPath = componentSource.match(/from ["']([^"']*\/composables\/useChoices\.ts[^"']*)["']/)?.[1]
      if (!choicesPath) throw new Error('BoardCard choices import not found')
      const { provideChoices } = await load(choicesPath)
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const state = reactive({
        selected: [] as number[], occupied: [0],
        choices: [{ index: 7, kind: 'ability', target: 'metagame-test', code: null, abilityType: 'HeroAction', label: '$label.metagameSlot count=i:5' }],
      })
      const resourceSlots = spaces === 9
        ? ['Energy', 'Energy', 'Energy', 'Mental', 'Mental', 'Mental', 'Physical', 'Physical', 'Physical']
        : ['Energy', 'Energy', 'Mental', 'Mental', 'Physical', 'Physical']
      const host = document.createElement('div')
      host.style.setProperty('--card-width', '100px')
      document.body.replaceChildren(host)
      createApp({ setup() {
        provideChoices(() => state.choices, (index: number) => {
          state.selected.push(index)
          state.occupied.push(4)
          state.choices = []
        })
        return () => h(BoardCard, {
          id: 'metagame-test', code: spaces === 9 ? '44057' : '44053',
          name: spaces === 9 ? 'Tic-Tac-Toe' : 'Blackout', resourceSlots,
          occupiedSlots: state.occupied,
        })
      } })
        .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } }))
        .mount(host)
      Object.assign(window, { metagameState: state })
    }, spaces)
    const board = page.getByLabel('Metagame board', { exact: true })
    await expect(board.locator('button')).toHaveCount(spaces)
    await expect(board.getByRole('button', { name: /Space 1:.*occupied/ })).toBeDisabled()
    await expect(board.getByRole('button', { name: /Space 2:.*empty/ })).toBeDisabled()
    const available = board.getByRole('button', { name: /Space 5:.*empty/ })
    await expect(available).toBeEnabled()
    await expect(page.locator('.chips .chip')).toHaveCount(0)
    const bounds = (await board.boundingBox())!
    const card = (await page.locator('.card').boundingBox())!
    expect(bounds.y + bounds.height).toBeLessThanOrEqual(card.y + card.height)
    await available.focus()
    await page.keyboard.press('Enter')
    await expect(board.getByRole('button', { name: /Space 5:.*occupied/ })).toBeDisabled()
    expect(await page.evaluate(() => (window as any).metagameState.selected)).toEqual([7])
  })
}
