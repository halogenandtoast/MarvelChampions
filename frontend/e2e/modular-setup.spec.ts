import { expect, test } from '@playwright/test'

const user = { id: 1, username: 'modular-test', email: 'modular@example.com', admin: false, beta: true, guest: false, allowSpectatorsByDefault: false }
const choices = ['BombScare', 'MastersOfEvil', 'UnderAttack', 'GoblinGimmicks', 'Temporal', 'Anachronauts', 'MasterOfTime']
const scenarios = [
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

for (const [tile, text] of [
  ['Rhino', "The core sets' difficulty ratings run from 1 to 5."],
  ['Risky Business', 'Each Green Goblin scenario calls for one modular encounter set'],
  ['The Wrecking Crew', "does not use other encounter sets"],
  ['The Rise of Red Skull', 'three for Crossbones, two for Red Skull'],
  ['The Once and Future Kang', 'Temporal (difficulty 4) for a first game'],
  ["The Galaxy's Most Wanted", 'Band of Badoon, Menagerie Medley, Space Pirates, or Kree Militants'],
  ["The Mad Titan's Shadow", 'one for Tower Defense, or two for Ebony Maw'],
  ['The Hood', 'Choose any seven modular encounter sets'],
]) {
  test(`shows product modular instructions for ${tile}`, async ({ page }) => {
    const label = tile === 'Rhino' ? 'Rhino Core Set' : tile === 'Risky Business' ? 'Risky Business The Green Goblin' : tile
    await page.getByRole('radio', { name: label, exact: true }).click()
    await expect(page.locator('.modular-guidance')).toContainText(text)
    await expect(page.locator('.modular-guidance > a')).toHaveAttribute('href', /\.pdf$/)
    if (tile === 'The Wrecking Crew') await expect(page.locator('.set-chip')).toHaveCount(0)
  })
}
