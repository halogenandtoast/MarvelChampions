import { inject, provide, ref, watch, type InjectionKey, type Ref } from 'vue'
import type { GameView } from '../server'

export interface DebugCard {
  id: string
  name: string
  code: string
  kind: string
  values: Record<string, number | boolean>
}
export const cardDebugKey: InjectionKey<{ enabled: Readonly<Ref<boolean>>; open: (id: string, card?: { code: string; name: string }) => void }> = Symbol.for('marvel.cardDebug')
export const useCardDebug = () => inject(cardDebugKey, null)

export const provideCardDebug = (game: Readonly<Ref<GameView | null>>, enabled: Readonly<Ref<boolean>>, debug: (message: unknown) => Promise<boolean>) => {
  const debugCard = ref<DebugCard | null>(null)
  watch(enabled, enabled => { if (!enabled) debugCard.value = null })
  provide(cardDebugKey, { enabled, open: (id, fallback) => {
    if (!enabled.value || !game.value) return
    const g = game.value
    const candidates: [string, unknown[]][] = [
      ['Identity', g.players], ['Villain', g.villains], ['Minion', g.minions],
      ['MainScheme', g.mainSchemes], ['SideScheme', g.sideSchemes],
      ['Environment', g.environments ?? []], ['Ally', g.uncontrolledAllies ?? []],
    ]
    for (const player of g.players) candidates.push(['Ally', player.allies], ['Support', player.supports], ['Upgrade', player.upgrades], ['Attachment', [...(player.attachments ?? []), ...(player.obligations ?? [])]])
    for (const host of [...g.villains, ...g.minions, ...g.mainSchemes, ...g.sideSchemes, ...g.players.flatMap(p => p.allies)]) {
      candidates.push(['Attachment', host.attachments ?? []], ['Upgrade', host.upgrades ?? []])
    }
    for (const [kind, entries] of candidates) {
      const card = (entries as Record<string, unknown>[]).find(card => card.id === id)
      if (!card) continue
      const fields: Record<string, string[]> = {
        Identity: ['damage', 'counters', 'exhausted'], Ally: ['damage', 'counters', 'exhausted'],
        Villain: ['damage', 'counters'], Minion: ['damage', 'counters', 'exhausted'],
        MainScheme: ['threat', 'counters', 'accelerationTokens'], SideScheme: ['threat', 'counters', 'accelerationTokens', 'exhausted'],
        Support: ['uses', 'threat', 'exhausted'], Upgrade: ['uses', 'exhausted'],
        Attachment: ['damage', 'counters', 'exhausted'], Environment: ['damage', 'counters'],
      }
      const values: DebugCard['values'] = {}
      for (const key of fields[kind] ?? []) values[key] = typeof card[key] === 'boolean' ? card[key] as boolean : typeof card[key] === 'number' ? card[key] as number : key === 'exhausted' ? false : 0
      if (['Identity', 'Ally', 'Villain', 'Minion'].includes(kind)) {
        for (const [key, count] of [['stunned', 'stunCards'], ['confused', 'confuseCards'], ['tough', 'toughCards']]) {
          values[key!] = (card[count!] as number | undefined) ?? (card[key!] ? 1 : 0)
        }
      }
      debugCard.value = { id, kind, code: card.code as string, name: card.name as string, values }
      return
    }
    if (fallback) debugCard.value = { id, ...fallback, kind: 'CardId', values: {} }
  } })
  const saveDebugCard = (card: DebugCard, values: DebugCard['values']) => {
    const patch = { ...values }
    for (const [status, additional] of [['stunned', 'additionalStun'], ['confused', 'additionalConfuse'], ['tough', 'additionalTough']]) {
      if (!(status! in patch)) continue
      const count = Number(patch[status!])
      if (!Number.isInteger(count) || count < 0 || (status !== 'tough' && count > 2) || (status === 'tough' && card.kind === 'Villain' && count > 1)) return Promise.resolve(false)
      patch[status!] = count > 0
      if (status !== 'tough' || card.kind !== 'Villain') patch[additional!] = status === 'tough' ? Math.max(0, count - 1) : count > 1
    }
    if (Object.values(patch).some(value => typeof value === 'number' && (!Number.isInteger(value) || value < 0))) return Promise.resolve(false)
    return debug({ tag: 'CardSpecific', contents: [{ tag: `${card.kind}Ref`, contents: card.id }, 'debugSetValues', patch] })
  }

  return { debugCard, saveDebugCard }
}
