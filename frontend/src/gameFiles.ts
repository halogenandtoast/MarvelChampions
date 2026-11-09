import type { GameSummary } from './server'

/** Inspect locally; no game is created until the player confirms the import. */
export async function inspectGameFile(file: File): Promise<{ preview: GameSummary | null; name: string }> {
  try {
    const magic = new Uint8Array(await file.slice(0, 2).arrayBuffer())
    const text = magic[0] === 0x1f && magic[1] === 0x8b
      ? await new Response(file.stream().pipeThrough(new DecompressionStream('gzip'))).text()
      : await file.text()
    const saved = JSON.parse(text)
    if (saved.version !== 1 || typeof saved.name !== 'string' || !saved.game || !Array.isArray(saved.steps) || !Array.isArray(saved.queue)) {
      throw new Error('Invalid export')
    }
    // Old JSON exports have no preview but remain loadable.
    const p = saved.preview
    const preview: GameSummary | null = p && typeof p.name === 'string' && typeof p.scenario === 'string'
      && typeof p.villain === 'string' && typeof p.round === 'number' && typeof p.phase === 'string'
      && Array.isArray(p.seats) && p.seats.every((s: Record<string, unknown>) => s && typeof s.name === 'string' && typeof s.identity === 'string')
      ? p : null
    return { preview, name: saved.name }
  } catch {
    throw new Error('$error.invalidGameFile')
  }
}
