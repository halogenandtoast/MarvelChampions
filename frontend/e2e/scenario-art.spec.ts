import { expect, test } from '@playwright/test'

test('Shadowland campaign uses full-size rectangular scenario artwork', async ({ page }) => {
  const user = { id: 123, username: 'fixture', email: 'fixture@example.test', admin: false, beta: true, guest: false, allowSpectatorsByDefault: false }
  await page.addInitScript(user => localStorage.setItem('marvel-session', JSON.stringify({ token: 'fixture', user })), user)
  await page.route('**/api/whoami', route => route.fulfill({ json: user }))
  await page.route('**/api/decks', route => route.fulfill({ json: [] }))
  await page.route('**/api/starter-decks', route => route.fulfill({ json: [] }))
  await page.route('**/api/campaigns/art-fixture', route => route.fulfill({ json: {
    id: 'art-fixture', name: 'Shadowland', difficulty: 'Normal', heroes: [],
    campaign: { kind: 'ShadowlandCampaign', mode: 'StandardCampaign', scenarios: ['65001a', '65038a', '65060a'], completed: 0, lost: false,
      campaignLog: { players: [], shadowland: { removedProteges: [], overpoweringHistory: [], preparedCards: [] } } },
    table: { owner: 'fixture', seats: 1, seated: [], withFriends: false },
    waitingForPlayers: false, deckShort: [], market: [], next: null, nextName: null, currentGame: null, won: false,
  } }))
  await page.goto('/#/campaigns/art-fixture')
  const art = page.locator('.track .art')
  await expect(art).toHaveCount(3)
  for (const tile of await art.all()) {
    const bounds = (await tile.boundingBox())!
    expect(bounds.width).toBeGreaterThan(100)
    expect(bounds.height).toBeGreaterThan(100)
    expect(bounds.width / bounds.height).toBeCloseTo(16 / 10, 1)
    await expect(tile).toHaveCSS('border-radius', '0px')
  }
  await expect(art.first()).toHaveCSS('background-image', /shadowland-1-001b\.avif/)
  await expect(art.first()).toHaveCSS('background-size', '230% auto')
  await expect(art.first()).toHaveCSS('background-position', '100% 45%')
  await expect(art.nth(1)).toHaveCSS('background-image', /shadowland-2-001a\.avif/)
  await expect(art.nth(2)).toHaveCSS('background-image', /shadowland-3-001\.avif/)
  await page.locator('.track').screenshot({ path: '/tmp/shadowland-campaign-art.png' })
  await page.getByRole('button', { name: 'Campaign guide', exact: true }).click()
  const guide = page.getByRole('dialog')
  await expect(guide.getByRole('heading', { name: 'Shadowland: Campaign Mode', exact: true })).toBeVisible()
  await expect(guide.getByRole('link', { name: 'Official rulebook' })).toHaveAttribute('href', /MC65-Rulebook\.pdf$/)
  await guide.locator('footer').getByRole('button', { name: 'Close', exact: true }).click()
  await expect(guide).toHaveCount(0)
  await expect(art).toHaveCount(3)
})

test('game rows use scenario illustrations instead of setup faces', async ({ page }) => {
  await page.goto('/#/cards/60121a')
  await expect(page.getByRole('heading', { name: 'Art Museum Heist', exact: true }).first()).toBeVisible()
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { createRouter, createMemoryHistory } = await load('/node_modules/.vite/deps/vue-router.js')
    const { default: GameRow } = await load('/src/components/GameRow.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ setup: () => () => ['60121a', '65060a'].map(villain => h(GameRow, {
      game: { id: villain, villain, name: villain, scenario: villain, difficulty: 'Normal', round: 1, phase: 'PlayerPhase', outcome: null, waiting: false, seats: [], playerName: '' },
    })) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } }))
      .use(createRouter({ history: createMemoryHistory(), routes: [{ path: '/:pathMatch(.*)*', component: { render: () => null } }] })).mount(host)
  })
  const portraits = page.locator('.game-row .villain')
  await expect(portraits.first()).toHaveClass(/scheme-art/)
  await expect(portraits.first()).toHaveCSS('background-image', /60121b\.avif/)
  await expect(portraits.first()).toHaveCSS('background-size', '230% auto')
  await expect(portraits.nth(1)).toHaveCSS('background-image', /shadowland-3-001\.avif/)
})

for (const width of [1280, 390]) {
  test(`campaign scheme choices stay horizontal at ${width}px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 720 })
    await page.goto('/#/cards/60121a')
    await expect(page.getByRole('heading', { name: 'Art Museum Heist', exact: true }).first()).toBeVisible()
    await page.evaluate(async () => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { default: CardPick } = await load('/src/components/CardPick.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      document.body.replaceChildren(host)
      host.id = 'scenario-choice-host'
      createApp({ setup: () => () => h(CardPick, {
        prompt: '$label.fneChooseCampaignScenario', canChoose: true,
        choices: ['60121a', '60128a', '60134a', '60142a', '60151a'].map((code, index) => ({ code, index, kind: 'card' })),
        onChoose: (index: number) => { host.dataset.chosen = String(index) },
      }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    })
    const dialog = page.getByRole('dialog')
    await expect(dialog.locator('.card')).toHaveCount(5)
    for (const face of await dialog.locator('.face').all()) {
      const bounds = (await face.boundingBox())!
      expect(bounds.width).toBeGreaterThan(bounds.height)
      expect(await face.locator('.scan').evaluate(el => getComputedStyle(el).rotate)).toBe('none')
    }
    const bounds = (await dialog.boundingBox())!
    expect(bounds.x).toBeGreaterThanOrEqual(0)
    expect(bounds.x + bounds.width).toBeLessThanOrEqual(width)
    await dialog.locator('.card').first().click()
    await expect(page.locator('#scenario-choice-host')).toHaveAttribute('data-chosen', '0')
  })
}
