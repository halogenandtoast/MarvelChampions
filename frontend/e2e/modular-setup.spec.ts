import { expect, test } from '@playwright/test'

const user = { id: 1, username: 'modular-test', email: 'modular@example.com', admin: false, beta: true, guest: false, allowSpectatorsByDefault: false }
const choices = ['StreetsOfMayhem', 'BeastyBoys', 'BrothersGrimm', 'CrossfiresCrew', 'MisterHydeSet', 'RansackedArmory', 'SinisterSyndicate', 'StateOfEmergency', 'WreckingCrewModular', 'Crime', 'Fantasy', 'Horror', 'SciFi', 'Sitcom', 'Western', 'LongshotSet', 'BombScare', 'MastersOfEvil', 'UnderAttack', 'GoblinGimmicks', 'Temporal', 'Anachronauts', 'MasterOfTime']
const scenarios = [
  { code: '39001a', modulars: [] },
  { code: '39012a', modulars: [] },
  { code: '01094', modulars: ['BombScare'] },
  { code: '01113', modulars: ['MastersOfEvil'] },
  { code: '01134', modulars: ['UnderAttack'] },
  { code: '02001a', modulars: ['GoblinGimmicks'] },
  { code: '02014', modulars: ['GoblinGimmicks'] },
  { code: '07001', modulars: [] },
  { code: '04058', modulars: ['HydraAssault', 'WeaponMaster', 'LegionsOfHydra'] },
  { code: '11001', modulars: ['Temporal'] },
  { code: '16058', modulars: ['BandOfBadoon'] },
  { code: '21071', modulars: ['BlackOrderSet', 'ArmiesOfTitan'] },
  { code: '24001', modulars: [] },
].map((s) => ({ ...s, name: s.code }))

test.beforeEach(async ({ page }) => {
  await page.addInitScript((user) => localStorage.setItem('marvel-session', JSON.stringify({ token: 'modular-test', user })), user)
  await page.route('**/api/whoami', (route) => route.fulfill({ json: user }))
  await page.route('**/api/scenarios', (route) => route.fulfill({ json: { scenarios, modulars: [...choices, 'HydraAssault', 'WeaponMaster', 'LegionsOfHydra', 'BandOfBadoon', 'BlackOrderSet', 'ArmiesOfTitan'] } }))
  await page.goto('/#/new-game')
})

test('the Hood has no recommended selection and requires seven deliberate choices', async ({ page }) => {
  await page.getByRole('radio', { name: 'The Hood', exact: true }).click()
  await expect(page.locator('.set-chip.on')).toHaveCount(0)
  await expect(page.locator('.set-chip small')).toHaveCount(0)
  await expect(page.getByText('0/7 modular sets selected.', { exact: false })).toBeVisible()
  const create = page.getByRole('button', { name: 'Open the lobby', exact: true })
  await expect(create).toBeDisabled()
  await page.locator('.modular-library summary').click()
  for (const name of ['Bomb Scare', 'Masters of Evil', 'Under Attack', 'Goblin Gimmicks', 'Temporal', 'Anachronauts']) {
    await page.getByRole('button', { name, exact: true }).click()
  }
  await expect(create).toBeDisabled()
  await page.getByRole('button', { name: 'Master of Time', exact: true }).click()
  await expect(create).toBeEnabled()
  await page.getByRole('button', { name: 'Bomb Scare', exact: true }).click()
  await expect(create).toBeDisabled()
  await page.getByText('The Hood: modular set difficulty guide', { exact: true }).click()
  await expect(page.locator('.modular-guidance ol li')).toHaveCount(9)
  await expect(page.locator('.modular-guidance ol li').first()).toHaveText('Streets of Mayhem')
  await expect(page.locator('.modular-guidance ol li').last()).toHaveText('Wrecking Crew')
})

test('The Hood shows all nine pack modulars in its own group', async ({ page }) => {
  await page.getByRole('radio', { name: 'The Hood', exact: true }).click()
  const product = page.locator('.modular-group').first()
  await expect(product).toHaveAttribute('aria-label', 'The Hood')
  await expect(product.getByRole('button')).toHaveCount(9)
  for (const name of ['Streets of Mayhem', 'Beasty Boys', 'Brothers Grimm', "Crossfire's Crew", 'Mister Hyde', 'Ransacked Armory', 'Sinister Syndicate', 'State of Emergency', 'Wrecking Crew']) {
    await expect(product.getByRole('button', { name, exact: true })).toBeVisible()
  }
  await page.getByLabel('Standard encounter set', { exact: true }).selectOption('StandardII')
  await expect(page.getByText('0/7 modular sets selected.', { exact: false })).toBeVisible()
})

test('printed recommendations remain selected for Rhino', async ({ page }) => {
  await page.getByRole('radio', { name: 'Rhino Core Set', exact: true }).click()
  await expect(page.locator('.set-chip.on')).toHaveCount(1)
  await expect(page.locator('.set-chip.on')).toContainText('Bomb Scare')
  await expect(page.locator('.set-chip.on small')).toHaveText('recommended')
  await expect(page.getByRole('button', { name: 'Masters of Evil', exact: true })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Temporal', exact: true })).toBeHidden()
  await page.locator('.modular-library summary').click()
  await page.getByRole('searchbox', { name: 'Search sets' }).fill('Temporal')
  await expect(page.locator('.modular-option')).toHaveCount(1)
  await page.getByRole('button', { name: 'Temporal', exact: true }).click()
  await page.locator('.modular-library summary').click()
  await expect(page.getByRole('button', { name: 'Temporal', exact: true })).toHaveAttribute('aria-pressed', 'true')
  await page.getByRole('button', { name: 'Temporal', exact: true }).click()
  await expect(page.locator('.set-chip.on')).toHaveCount(1)
})

test('MaGog shows its random genre pool separately and allows a specific choice', async ({ page }) => {
  await page.getByRole('radio', { name: 'Magog Mojo Mania', exact: true }).click()
  const random = page.getByRole('button', { name: 'Random genre (recommended)', exact: true })
  await expect(random).toHaveAttribute('aria-pressed', 'true')
  await expect(page.getByText('One of these six genre sets is chosen at random when the game starts.')).toBeVisible()
  await page.getByRole('button', { name: 'Fantasy', exact: true }).click()
  await expect(random).toHaveAttribute('aria-pressed', 'false')
  await page.getByRole('button', { name: 'Horror', exact: true }).click()
  await expect(page.getByRole('button', { name: 'Fantasy', exact: true })).toHaveAttribute('aria-pressed', 'false')
  await random.click()
  await expect(random).toHaveAttribute('aria-pressed', 'true')
})

test('sets from the scenario product stay visible without opening the library', async ({ page }) => {
  await page.getByRole('radio', { name: 'The Once and Future Kang', exact: true }).click()
  await expect(page.locator('.modular-group').first()).toHaveAttribute('aria-label', 'The Once and Future Kang')
  await expect(page.locator('.modular-group').nth(1)).toHaveAttribute('aria-label', 'Core Set')
  await expect(page.getByRole('button', { name: 'Anachronauts', exact: true })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Master of Time', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'Anachronauts', exact: true }).click()
  await page.getByRole('button', { name: 'Anachronauts', exact: true }).click()
  await expect(page.getByRole('button', { name: 'Anachronauts', exact: true })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Goblin Gimmicks', exact: true })).toBeHidden()
})

for (const [tile, text] of [
  ['Rhino', "You can swap the recommended modular sets for others."],
  ['Risky Business', 'You can swap the recommended modular sets for others.'],
  ['The Wrecking Crew', "does not use modular sets"],
  ['The Rise of Red Skull', 'You can swap the recommended modular sets for others.'],
  ['The Once and Future Kang', 'Temporal (difficulty 4)'],
  ["The Galaxy's Most Wanted", 'You can swap the recommended modular sets for others.'],
  ["The Mad Titan's Shadow", 'You can swap the recommended modular sets for others.'],
  ['The Hood', 'Choose seven modular sets'],
]) {
  test(`shows product modular instructions for ${tile}`, async ({ page }) => {
    const label = tile === 'Rhino' ? 'Rhino Core Set' : tile === 'Risky Business' ? 'Risky Business The Green Goblin' : tile
    await page.getByRole('radio', { name: label, exact: true }).click()
    await expect(page.locator('.modular-guidance')).toContainText(text)
    await expect(page.locator('.modular-guidance > a')).toHaveAttribute('href', /\.pdf$/)
    if (tile === 'The Wrecking Crew') await expect(page.locator('.set-chip')).toHaveCount(0)
  })
}
