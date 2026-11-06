import { assetUrl } from './assets'
import sources from '../public/rulebooks/sources.json'

const localCopies = new Map(Object.entries(sources).flatMap(([filename, entry]) =>
  [entry.source, ...entry.aliases].map(source => [source, assetUrl(`/rulebooks/${filename}`)] as const),
))

/** Keep original source attribution in the card data while opening our local copy. */
export const localRulebookUrl = (source: string) => {
  const [url, fragment] = source.split('#', 2)
  const local = localCopies.get(url!)
  return local ? `${local}${fragment ? `#${fragment}` : ''}` : source
}
