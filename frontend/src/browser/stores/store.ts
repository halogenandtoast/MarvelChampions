import { reactive, type UnwrapNestedRefs } from 'vue'

/**
 * A setup store without Pinia: one shared instance, its refs unwrapped on access the
 * way Pinia's are, so the browser's components read it the same.
 */
export function defineStore<T extends object>(_id: string, setup: () => T): () => UnwrapNestedRefs<T> {
  let store: UnwrapNestedRefs<T> | undefined
  return () => (store ??= reactive(setup()))
}
