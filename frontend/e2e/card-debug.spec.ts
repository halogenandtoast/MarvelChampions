import { expect, test, type Page } from '@playwright/test'

const mount = async (page: Page) => {
  await page.goto('/#/cards/37001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h, ref } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: BoardCard } = await load('/src/components/BoardCard.vue')
    const { default: CardDebugDialog } = await load('/src/components/CardDebugDialog.vue')
    const { provideCardDebug } = await load('/src/composables/useCardDebug.ts')
    const { provideChoices } = await load('/src/composables/useChoices.ts')
    const { useCards } = await load('/src/cards.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    await useCards().store.load()
    const { messages } = await loadLocaleMessages('en')
    const enabled = ref(true)
    const state = { sent: [] as unknown[], answers: [] as number[], fail: false }
    const host = document.createElement('div')
    host.style.setProperty('--card-width', '108px')
    document.body.replaceChildren(host)
    createApp({ setup: () => {
      const { debugCard, saveDebugCard } = provideCardDebug(ref({
        players: [{ id: 'hero', code: '37001a', name: 'Gambit', damage: 2, counters: 3, exhausted: false, stunned: true, stunCards: 2, confused: false, tough: false, allies: [], supports: [], upgrades: [] }],
        villains: [], minions: [], mainSchemes: [{ id: 'scheme', code: '01097', name: 'The Break-In!', threat: 4, counters: 0, accelerationTokens: 1 }], sideSchemes: [],
      }), enabled, async (message: unknown) => { state.sent.push(message); return !state.fail })
      provideChoices(() => [{ index: 0, kind: 'ability', target: 'hero', label: '$label.attack' }], (index: number) => state.answers.push(index))
      return () => [h(BoardCard, { id: 'hero', code: '37001a', name: 'Gambit' }), h(BoardCard, { id: 'scheme', code: '01097', name: 'The Break-In!' }), h(CardDebugDialog, { card: debugCard.value, save: saveDebugCard, onClose: () => { debugCard.value = null } })]
    } }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    Object.assign(window, { debugState: state, debugEnabled: enabled })
  })
}

for (const width of [390, 1600]) {
  test(`long press edits a card without choosing its action at ${width}px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 844 })
    await mount(page)
    const card = page.locator('[data-card-id="hero"]')
    await card.dispatchEvent('pointerdown', { pointerType: 'touch', clientX: 30, clientY: 30 })
    const dialog = page.getByRole('dialog')
    await expect(dialog).toBeVisible()
    await card.dispatchEvent('pointerup', { pointerType: 'touch' })
    await card.dispatchEvent('click')
    await expect(dialog.getByLabel('Damage', { exact: true })).toHaveValue('2')
    await expect(dialog.getByLabel('Stunned cards (0–2)')).toHaveValue('2')
    await dialog.getByLabel('Damage', { exact: true }).fill('5')
    await dialog.getByLabel('Counters', { exact: true }).fill('8')
    await dialog.getByLabel('Confused cards (0–2)').fill('1')
    await dialog.getByLabel('Exhausted', { exact: true }).check()
    await dialog.getByRole('button', { name: 'Apply changes' }).click()
    await expect(dialog).not.toBeVisible()
    const state = await page.evaluate(() => (window as any).debugState)
    expect(state.answers).toEqual([])
    expect(state.sent).toEqual([{ tag: 'CardSpecific', contents: [{ tag: 'IdentityRef', contents: 'hero' }, 'debugSetValues', { damage: 5, counters: 8, exhausted: true, stunned: true, confused: true, tough: false, additionalStun: true, additionalConfuse: false, additionalTough: 0 }] }])
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true)
  })
}

test('drag cancels debug long press and disabled mode preserves ordinary choices', async ({ page }) => {
  await mount(page)
  const card = page.locator('[data-card-id="hero"]')
  await card.dispatchEvent('pointerdown', { pointerType: 'touch', clientX: 30, clientY: 30 })
  await card.dispatchEvent('pointermove', { pointerType: 'touch', clientX: 60, clientY: 30 })
  await page.waitForTimeout(600)
  await expect(page.getByRole('dialog')).not.toBeVisible()
  await page.evaluate(() => { (window as any).debugEnabled.value = false })
  await card.dispatchEvent('pointerdown', { pointerType: 'mouse' })
  await card.dispatchEvent('pointerup', { pointerType: 'mouse' })
  await card.click()
  expect(await page.evaluate(() => (window as any).debugState.answers)).toEqual([0])
})

test('keyboard editing exposes scheme tokens and keeps a failed edit open', async ({ page }) => {
  await mount(page)
  await page.locator('[data-card-id="scheme"]').press('Shift+F10')
  const dialog = page.getByRole('dialog')
  await expect(dialog).toBeVisible()
  await expect(dialog.getByLabel('Threat', { exact: true })).toHaveValue('4')
  await expect(dialog.getByLabel('Acceleration tokens')).toHaveValue('1')
  await page.evaluate(() => { (window as any).debugState.fail = true })
  await dialog.getByRole('button', { name: 'Apply changes' }).click()
  await expect(dialog.getByRole('alert')).toBeVisible()
  await expect(dialog).toBeVisible()
  await page.evaluate(() => { (window as any).debugEnabled.value = false })
  await expect(dialog).not.toBeVisible()
})
