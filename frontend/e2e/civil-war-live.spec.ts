import { expect, test } from '@playwright/test'

// Start the compiled API against a disposable database, then a Vite instance
// proxying to that API. Ordinary frontend checks skip this integration test.
const api = process.env.CW_LIVE_API
const ui = process.env.CW_LIVE_UI

for (const seats of [2, 4]) {
test(`live Civil War starts ${seats} seats under one account and renders both boards`, async ({ page, request }) => {
  test.skip(!api || !ui, 'Requires CW_LIVE_API and CW_LIVE_UI for an isolated live stack')
  const sessionResponse = await request.post(`${api}/guest`, { data: {} })
  expect(sessionResponse.ok()).toBeTruthy()
  const session = await sessionResponse.json()
  const headers = { Authorization: `Token ${session.token}` }
  const optionsResponse = await request.get(`${api}/scenarios`, { headers })
  const { civilWar } = await optionsResponse.json()
  const registration = civilWar.registration.presets[0]
  const resistance = civilWar.resistance.presets[0]
  const created = await request.post(`${api}/games`, { headers, data: {
    name: 'Civil War live browser verification', seats, decks: [],
    scenario: { scenarioCode: registration.leader, modulars: registration.modulars, difficulty: 'Normal', competition: { registration, resistance } },
  } })
  expect(created.ok()).toBeTruthy()
  const { id } = await created.json()
  for (const [position, starter] of (seats === 2 ? ['spider-man', 'thor'] : ['spider-man', 'thor', 'black-panther', 'hawkeye']).entries()) {
    expect((await request.post(`${api}/games/${id}/join`, { headers, data: { position, deck: { starter } } })).ok()).toBeTruthy()
  }
  expect((await request.post(`${api}/games/${id}/start`, { headers, data: {} })).ok()).toBeTruthy()
  let initialized = false
  for (let step = 0; step < 80; step++) {
    const state = await (await request.get(`${api}/games/${id}`, { headers })).json()
    const game = state.game
    if (game.villains.length === 2 && game.mainSchemes.length === 2 && game.mainSchemes.every((scheme: { code: string }) => !scheme.code.endsWith('a')) && Object.values(game.question).some((question: any) => question.choices.some((choice: any) => choice.kind === 'endTurn'))) { initialized = true; break }
    const [player, question] = Object.entries(game.question)[0] as [string, { choices: { index: number; label: string }[] }]
    const done = question.choices.find((choice) => /done|keep|finish|noResponse|noDefender/i.test(choice.label ?? ''))
    const choice = done ?? question.choices[0]
    expect((await request.post(`${api}/games/${id}`, { headers, data: { player, questionVersion: game.questionVersion, choice: choice.index } })).ok()).toBeTruthy()
  }
  expect(initialized).toBeTruthy()
  await page.addInitScript((session) => localStorage.setItem('marvel-session', JSON.stringify(session)), session)
  const errors: string[] = []
  page.on('pageerror', (error) => errors.push(error.message))
  await page.goto(`${ui}/#/games/${id}`)
  const status = page.getByRole('navigation', { name: 'Team status' })
  await expect(status).toBeVisible()
  await expect(status.getByRole('button')).toHaveCount(2)
  await status.getByRole('button', { name: /Resistance/ }).click()
  await expect(status.getByRole('button', { name: /Resistance/ })).toHaveAttribute('aria-pressed', 'true')
  await expect(page.locator('.game-bar .phases li')).toHaveCount(4)
  await expect(page.locator('.game-bar .phases [aria-current="step"]')).toHaveText('Registration · Player')
  await expect(page.locator('.game-bar .turn')).toBeVisible()
  await expect(page.locator('.villain-area .villain').first()).toBeVisible()
  await page.waitForLoadState('networkidle')
  await expect(page.locator('.villain-area .placeholder')).toHaveCount(0)
  await page.screenshot({ path: `/tmp/mc-cw-live-board-${seats}.png`, fullPage: true })
  expect(errors).toEqual([])
  expect((await request.delete(`${api}/games/${id}`, { headers })).ok()).toBeTruthy()
})

}
