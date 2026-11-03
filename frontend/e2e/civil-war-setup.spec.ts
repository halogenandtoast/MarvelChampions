import { expect, test } from '@playwright/test'

const user = { id: 1, username: 'civil-war-test', email: 'cw@example.com', admin: false, beta: true, guest: false, allowSpectatorsByDefault: false }
const registration = { leader: '56059', modulars: ['CwMightyAvengers', 'CwTheInitiative', 'CwMariaHillModular', 'CwDangerousRecruits'], mainSchemes: ['56063b', '56064b'] }
const resistance = { leader: '56137', modulars: ['CwNewAvengers', 'CwSecretAvengers', 'CwNamorModular', 'CwAtlanteans'], mainSchemes: ['56141b', '56142b'] }

test('competitive setup creates a four-seat lobby with independent faction scenarios', async ({ page }) => {
  await page.addInitScript((user) => localStorage.setItem('marvel-session', JSON.stringify({ token: 'cw-test', user })), user)
  await page.route('**/api/whoami', (route) => route.fulfill({ json: user }))
  await page.route('**/api/scenarios', (route) => route.fulfill({ json: {
    scenarios: [], modulars: [], civilWar: {
      registration: { presets: [registration], modulars: registration.modulars, firstSchemes: [registration.mainSchemes[0]], secondSchemes: [registration.mainSchemes[1]] },
      resistance: { presets: [resistance], modulars: resistance.modulars, firstSchemes: [resistance.mainSchemes[0]], secondSchemes: [resistance.mainSchemes[1]] },
    },
  } }))
  // Inspect the submitted setup while keeping the browser on the review screen.
  let submitted: any = null
  await page.route('**/api/games', async (route) => {
    submitted = route.request().postDataJSON()
    await route.fulfill({ status: 400, body: 'Test submission captured' })
  })
  await page.goto('/#/new-game/civil-war')
  const create = page.getByRole('button', { name: 'Create team lobby' })
  await expect(create).toBeEnabled()
  await page.getByRole('radio', { name: '4 · Two per team' }).click()
  const firstFaction = page.locator('fieldset').first()
  await firstFaction.getByRole('checkbox').nth(0).uncheck()
  await expect(create).toBeEnabled()
  await firstFaction.getByRole('checkbox').nth(1).uncheck()
  await expect(create).toBeDisabled()
  await firstFaction.getByRole('checkbox').nth(1).check()
  await create.click()
  await expect.poll(() => submitted).not.toBeNull()
  expect(submitted.seats).toBe(4)
  expect(submitted.decks).toEqual([])
  expect(submitted.scenario.competition.resistance).toEqual(resistance)
  expect(submitted.scenario.competition.registration.modulars).toHaveLength(3)
  expect(submitted.scenario.competition.registration.mainSchemes).toEqual(registration.mainSchemes)
})

test('the host can choose the opposing team seat and cannot start with an empty seat', async ({ page }) => {
  await page.addInitScript((user) => localStorage.setItem('marvel-session', JSON.stringify({ token: 'cw-test', user })), user)
  await page.route('**/api/whoami', (route) => route.fulfill({ json: user }))
  await page.route('**/api/decks', (route) => route.fulfill({ json: [] }))
  await page.route('**/api/starter-decks', (route) => route.fulfill({ json: [
    { key: 'spider-man', hero: '01001a', aspects: ['Aggression'] },
    { key: 'iron-man', hero: '01029a', aspects: ['Leadership'] },
  ] }))
  await page.goto('/#/cards/01001a')
  await page.evaluate(async ({ registration, resistance }) => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: Lobby } = await load('/src/components/Lobby.vue')
    const { router } = await load('/src/router.ts')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    Object.assign(window, { selectedSeat: null })
    createApp({ render: () => h(Lobby, {
      table: { owner: 1, withFriends: true, seats: 2, competition: { registration, resistance }, seated: [{ position: 0, userId: 1, username: 'civil-war-test', hero: '01001a' }] },
      onJoin: (deck: unknown, position: number) => Object.assign(window, { selectedSeat: { deck, position } }),
    }) }).use(router).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  }, { registration, resistance })
  await expect(page.getByRole('button', { name: /Start/ })).toBeDisabled()
  await expect(page.getByLabel('Choose your team and seat')).toHaveValue('1')
  await expect(page.getByText('Your team leader is Captain America. Choose a hero with a different title.')).toBeVisible()
  await expect(page.getByRole('radio', { name: /Spider-Man/ })).toBeEnabled()
  await page.getByRole('radio', { name: /Spider-Man/ }).click()
  expect(await page.evaluate(() => (window as any).selectedSeat)).toEqual({ deck: { starter: 'spider-man' }, position: 1 })
})

test('cooperative Civil War requires three or four modular sets', async ({ page }) => {
  await page.addInitScript((user) => localStorage.setItem('marvel-session', JSON.stringify({ token: 'cw-test', user })), user)
  await page.route('**/api/whoami', (route) => route.fulfill({ json: user }))
  await page.route('**/api/scenarios', (route) => route.fulfill({ json: {
    scenarios: [{ code: registration.leader, name: 'Iron Man', modulars: registration.modulars }],
    modulars: [...registration.modulars, ...resistance.modulars],
    civilWar: {
      registration: { presets: [registration], modulars: registration.modulars, firstSchemes: [], secondSchemes: [] },
      resistance: { presets: [resistance], modulars: resistance.modulars, firstSchemes: [], secondSchemes: [] },
    },
  } }))
  await page.goto('/#/new-game')
  await page.locator('.villains button').first().click()
  await expect(page.getByText('Cooperative Civil War:', { exact: false })).toBeVisible()
  const create = page.getByRole('button', { name: 'Open the lobby', exact: true })
  await expect(create).toBeEnabled()
  await page.locator('.set-chip.on').first().click()
  await expect(create).toBeEnabled()
  await page.locator('.set-chip.on').first().click()
  await expect(create).toBeDisabled()
  await expect(page.getByRole('button', { name: 'CwNewAvengers', exact: true })).toHaveCount(0)
})

test('competitive setup stays within a narrow viewport', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 })
  await page.addInitScript((user) => localStorage.setItem('marvel-session', JSON.stringify({ token: 'cw-test', user })), user)
  await page.route('**/api/whoami', (route) => route.fulfill({ json: user }))
  await page.route('**/api/scenarios', (route) => route.fulfill({ json: {
    scenarios: [], modulars: [], civilWar: Object.fromEntries(Object.entries({ registration, resistance }).map(([faction, preset]) => [faction, {
      presets: [preset], modulars: preset.modulars, firstSchemes: [preset.mainSchemes[0]], secondSchemes: [preset.mainSchemes[1]],
    }])),
  } }))
  await page.goto('/#/new-game/civil-war')
  await expect(page.getByRole('button', { name: 'Create team lobby' })).toBeEnabled()
  expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(390)
  await page.screenshot({ path: '/tmp/mc-cw-creation-mobile.png', fullPage: true })
})

test('team summary shows reply results and selects the corresponding board', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: CompetitionStatus } = await load('/src/components/CompetitionStatus.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    Object.assign(window, { selectedArea: null })
    createApp({ render: () => h(CompetitionStatus, {
      shownArea: 0,
      competition: { phase: 'ResistancePlayerPhase', replyPending: true, teams: [
        { team: 'Registration', area: 0, roster: [{ name: 'Peter' }], remainingHeroHitPoints: 8, advancedMainScheme: false, mainThreat: 3, outcome: 'EnemyLeaderDefeated' },
        { team: 'Resistance', area: 1, roster: [{ name: 'Thor' }], remainingHeroHitPoints: 14, advancedMainScheme: true, mainThreat: 5, outcome: null },
      ] },
      onShow: (area: number) => Object.assign(window, { selectedArea: area }),
    }) }).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  const registrationTeam = page.getByRole('button', { name: /Registration/ })
  await expect(registrationTeam).toContainText('Enemy leader defeated')
  await expect(registrationTeam).toHaveAttribute('aria-pressed', 'true')
  await page.getByRole('button', { name: /Resistance/ }).click()
  expect(await page.evaluate(() => (window as any).selectedArea)).toBe(1)
})

test('competitive results distinguish both winners, a draw and both teams losing', async ({ page }) => {
  await page.goto('/#/cards/01001a')
  await page.evaluate(async () => {
    const load = (path: string) => import(/* @vite-ignore */ path)
    const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
    const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
    const { default: QuestionZone } = await load('/src/components/QuestionZone.vue')
    const { loadLocaleMessages } = await load('/src/locales/messages.ts')
    const { messages } = await loadLocaleMessages('en')
    const host = document.createElement('div')
    document.body.replaceChildren(host)
    createApp({ render: () => h('div', ['RegistrationWon', 'ResistanceWon', 'CompetitiveDraw', 'BothTeamsLost'].map((outcome) => h(QuestionZone, { outcome }))) })
      .use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
  })
  for (const text of ['Registration wins', 'Resistance wins', 'Draw', 'Both teams lose']) await expect(page.getByText(text, { exact: true })).toBeVisible()
  await expect(page.locator('.competitivedraw .lucide-minus')).toHaveCount(1)
  await expect(page.locator('.bothteamslost .lucide-skull')).toHaveCount(1)
})
