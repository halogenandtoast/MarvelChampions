import english from './en'
import base from './zh-cn/base.json'
import game from './zh-cn/game.json'
import browser from './zh-cn/browser.json'
import error from './zh-cn/error.json'
import label from './zh-cn/label.json'
import log from './zh-cn/log.json'

// Less common card-specific choices and campaign prose retain English fallback
// until reviewed translations are available; the complete game log is localized.
export default {
  ...english,
  ...base,
  game,
  browser,
  error,
  label: { ...english.label, ...label },
  log,
}
