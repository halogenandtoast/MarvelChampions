import { expect, test } from '@playwright/test'

const user = { id: 123, username: 'fixture', email: 'fixture@example.test', admin: false, beta: true, guest: false, allowSpectatorsByDefault: false }
const campaignId = 'campaign-fne'
const campaign = {
  id: campaignId, name: 'Our street campaign', currentGame: 'current-scenario', next: '60121a', nextName: 'Art Museum Heist', won: false,
  campaign: { kind: 'FearNoEvilCampaign', mode: 'ExpertCampaign', scenarios: ['60121a'], completed: 0, lost: false,
    campaignLog: { fearNoEvil: { resolved: [['60134a', false]], victorious: false } } },
  table: { owner: user.id, seats: 1, seated: [], withFriends: false },
}
const waitingCampaign = {
  ...campaign, id: 'campaign-waiting', name: 'Next adventure', currentGame: null, next: '65001a', nextName: 'Shadows in the Night',
  campaign: { kind: 'ShadowlandCampaign', mode: 'StandardCampaign', scenarios: ['65001a', '65038a', '65060a'], completed: 0, lost: false, campaignLog: {} },
}
const games = [
  { id: 'current-scenario', campaignId, name: `${campaign.name} – Art Museum Heist`, scenario: 'Art Museum Heist', villain: '60121a', outcome: null },
  { id: 'older-scenario', campaignId, name: `${campaign.name} – Protection Racket`, scenario: 'Protection Racket', villain: '60134a', outcome: 'Lost' },
  { id: 'standalone', campaignId: null, name: 'Just Rhino', scenario: 'Rhino', villain: '01094', outcome: null },
].map(game => ({ ...game, playerName: 'Matt', difficulty: 'Normal', round: 2, phase: 'PlayerPhase', waiting: false, seats: [] }))

async function setup(page: import('@playwright/test').Page, owner = true) {
  await page.addInitScript(user => localStorage.setItem('marvel-session', JSON.stringify({ token: 'fixture', user })), user)
  await page.route('**/api/whoami', route => route.fulfill({ json: user }))
  await page.route('**/api/games', route => route.fulfill({ json: games }))
  await page.route('**/api/campaigns', route => route.fulfill({ json: [
    { ...campaign, table: { ...campaign.table, owner: owner ? user.id : 456 } }, waitingCampaign,
  ] }))
  await page.route('**/api/spectatable', route => route.fulfill({ json: [] }))
  await page.goto('/#/')
}

for (const width of [1280, 390]) {
  test(`campaign metadata sits above the scenario at ${width}px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 900 })
    await setup(page)
    const row = page.locator('.game-row').filter({ has: page.locator('.scenario-link[href="#/games/current-scenario"]') })
    await expect(row.locator('.campaign-bar')).toHaveCount(1)
    await expect(page.locator('.campaign-link strong').filter({ hasText: campaign.name })).toHaveCount(1)
    await expect(row.locator('.campaign-bar')).toContainText('Expert campaign')
    await expect(row.locator('.campaign-bar')).toContainText('1 of 6 resolved')
    await expect(row.locator('.title')).toHaveText('Art Museum Heist')
    await expect(row.locator('.campaign-bar .lucide-chevron-right')).toHaveCount(0)
    await expect(row.locator('.scenario-link .select-chevron')).toHaveCount(1)
    await expect(row.locator('.game-content').getByRole('button', { name: 'Delete game' })).toHaveCount(0)
    await expect(row.locator('.campaign-bar').getByRole('button', { name: 'Delete campaign' })).toHaveCount(1)
    const barBounds = (await row.locator('.campaign-bar').boundingBox())!
    const bodyBounds = (await row.locator('.game-content').boundingBox())!
    expect(barBounds.y + barBounds.height).toBeLessThanOrEqual(bodyBounds.y + 1)
    expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(width)
    await expect(page.locator('.campaign-bar.standalone')).toContainText('Next adventure')
    await expect(page.locator('.campaign-bar.standalone')).toContainText('Shadows in the Night')
    await row.screenshot({ path: `/tmp/campaign-row-${width}.png` })
    await row.locator('.select-chevron').click()
    await expect(page).toHaveURL(/#\/games\/current-scenario$/)
  })
}

test('finished scenarios keep their campaign bar and standalone games keep game deletion', async ({ page }) => {
  await setup(page)
  const standalone = page.locator('.game-row').filter({ hasText: 'Just Rhino' })
  await expect(standalone.locator('.campaign-bar')).toHaveCount(0)
  await expect(standalone.getByRole('button', { name: 'Delete game' })).toBeVisible()
  await page.locator('.tabs label').filter({ hasText: 'Finished' }).click()
  const old = page.locator('.game-row')
  await expect(old.locator('.title')).toHaveText('Protection Racket')
  await expect(old.locator('.campaign-link')).toHaveAttribute('href', `#/campaigns/${campaignId}`)
  await expect(page.locator('.campaign-link strong').filter({ hasText: campaign.name })).toHaveCount(1)
})

test('campaign deletion confirms and removes every related scenario without a game-delete request', async ({ page }) => {
  await setup(page)
  const deletes: string[] = []
  await page.route('**/api/campaigns/campaign-fne', async route => {
    deletes.push(`${route.request().method()} ${new URL(route.request().url()).pathname}`)
    await route.fulfill({ status: 204 })
  })
  const row = page.locator('.game-row').filter({ hasText: 'Art Museum Heist' })
  await row.getByRole('button', { name: 'Delete campaign', exact: true }).click()
  await expect(page).toHaveURL(/#\/$/)
  const dialog = page.getByRole('dialog', { name: 'Delete this campaign and all its games?' })
  await expect(dialog).toContainText(campaign.name)
  await dialog.getByRole('button', { name: 'Cancel', exact: true }).click()
  expect(deletes).toEqual([])
  await row.getByRole('button', { name: 'Delete campaign', exact: true }).click()
  await dialog.getByRole('button', { name: 'Delete', exact: true }).click()
  await expect(page.locator('.game-row')).toHaveCount(1)
  expect(deletes).toEqual([`DELETE /api/campaigns/${campaignId}`])
  await expect(page.locator('.game-row')).toContainText('Just Rhino')
  await expect(page.locator('.tabs label').filter({ hasText: 'Finished' })).toContainText('0')
  await expect(page.locator('.campaign-link strong').filter({ hasText: campaign.name })).toHaveCount(0)
})

test('campaign members cannot delete a campaign they do not own', async ({ page }) => {
  await setup(page, false)
  const row = page.locator('.game-row').filter({ hasText: 'Art Museum Heist' })
  await expect(row.locator('.campaign-link')).toHaveAttribute('href', `#/campaigns/${campaignId}`)
  await expect(row.getByRole('button', { name: /Delete/ })).toHaveCount(0)
})

for (const inCampaign of [true, false]) {
  test(`in-game campaign navigation is ${inCampaign ? 'available for campaign scenarios' : 'hidden for standalone games'}`, async ({ page }) => {
    await page.goto('/#/cards/01001a')
    await page.evaluate(async inCampaign => {
      const load = (path: string) => import(/* @vite-ignore */ path)
      const { createApp, h } = await load('/node_modules/.vite/deps/vue.js')
      const { createI18n } = await load('/node_modules/.vite/deps/vue-i18n.js')
      const { createRouter, createMemoryHistory } = await load('/node_modules/.vite/deps/vue-router.js')
      const { default: GameBar } = await load('/src/components/GameBar.vue')
      const { loadLocaleMessages } = await load('/src/locales/messages.ts')
      const { messages } = await loadLocaleMessages('en')
      const host = document.createElement('div')
      document.body.replaceChildren(host)
      host.id = 'game-menu-host'
      const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/:pathMatch(.*)*', component: { render: () => null } }] })
      router.afterEach(to => { host.dataset.route = to.path })
      createApp({ setup: () => () => h(GameBar, {
        campaignId: inCampaign ? 'campaign-fne' : null,
        canUndo: false, showSidebar: false, spectate: false,
      }) }).use(router).use(createI18n({ legacy: false, locale: 'en', messages: { en: messages } })).mount(host)
    }, inCampaign)
    const link = page.getByRole('link', { name: 'Campaign', exact: true })
    if (inCampaign) {
      await expect(link).toHaveAttribute('href', '/campaigns/campaign-fne')
      await link.click()
      await expect(page.locator('#game-menu-host')).toHaveAttribute('data-route', '/campaigns/campaign-fne')
    } else {
      await expect(link).toHaveCount(0)
    }
  })
}
