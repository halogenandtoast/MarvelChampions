<script setup lang="ts">
import { computed, useTemplateRef } from 'vue'
import { BookOpen, Trash2 } from 'lucide-vue-next'
import { useCards } from '../cards'
import ConfirmDelete from './ConfirmDelete.vue'
import type { CampaignView } from '../server'

const { campaign, standalone = false, deleteCampaign } = defineProps<{ campaign: CampaignView; standalone?: boolean; deleteCampaign?: () => void }>()
const confirm = useTemplateRef<InstanceType<typeof ConfirmDelete>>('confirm')
const cards = useCards()
const fearNoEvil = computed(() => campaign.campaign.kind === 'FearNoEvilCampaign')
const progress = computed(() => ({
  done: fearNoEvil.value
    ? campaign.campaign.campaignLog.fearNoEvil.resolved.length + (campaign.campaign.campaignLog.fearNoEvil.victorious ? 1 : 0)
    : campaign.campaign.completed,
  total: fearNoEvil.value ? 6 : campaign.campaign.scenarios.length,
}))
const next = computed(() => campaign.nextName ?? (campaign.next ? cards.name(campaign.next, campaign.next) : null))
</script>

<template>
  <div class="campaign-bar" :class="{ standalone }">
    <RouterLink :to="`/campaigns/${campaign.id}`" class="campaign-link">
      <BookOpen class="book" aria-hidden="true" />
      <strong>{{ campaign.name }}</strong>
      <span>{{ $t(`campaign.${campaign.campaign.mode}`) }}</span>
      <span>{{ $t(fearNoEvil ? 'campaign.resolvedProgress' : 'campaign.progress', progress) }}</span>
      <span v-if="standalone && next" class="next">{{ $t('campaign.next') }}: {{ next }}</span>
      <span v-else-if="standalone && campaign.won" class="next">{{ $t('home.won') }}</span>
      <span v-else-if="standalone && campaign.campaign.lost" class="next">{{ $t('home.lost') }}</span>
    </RouterLink>
    <button v-if="deleteCampaign" type="button" class="delete-campaign" :aria-label="$t('campaign.delete')" :title="$t('campaign.delete')" @click="confirm?.open()"><Trash2 aria-hidden="true" /></button>
    <ConfirmDelete v-if="deleteCampaign" ref="confirm" :title="$t('campaign.confirmDelete')" @confirm="deleteCampaign()">
      <strong>{{ campaign.name }}</strong>
    </ConfirmDelete>
  </div>
</template>

<style scoped>
.campaign-bar { display: flex; align-items: center; border-bottom: 1px solid var(--line); border-left: 4px solid var(--edge, var(--crimson)); background: var(--ink-3); font-size: 0.75rem; line-height: 1.4; }
.campaign-link { flex: 1; min-width: 0; display: flex; flex-wrap: wrap; align-items: center; gap: 6px 14px; padding: 10px 14px; color: var(--text-muted); text-decoration: none; }
.campaign-link:hover { background: var(--ink-4); color: var(--text); }
strong { color: var(--text); font-size: 0.85rem; overflow-wrap: anywhere; }
svg { flex: none; width: 15px; height: 15px; }
.book { color: var(--amber); }
.next { color: var(--amber); margin-left: auto; }
.standalone { border: 1px solid var(--line); border-left: 4px solid var(--crimson); border-radius: 14px; overflow: hidden; }
.standalone .campaign-link { padding-block: 14px; }
.delete-campaign { flex: none; display: grid; place-items: center; margin: 0 10px 0 0; padding: 8px; border: 0; border-radius: 8px; background: none; color: var(--text-faint); cursor: pointer; }
.delete-campaign:hover { color: var(--danger); background: rgb(214 80 88 / 0.1); }
.delete-campaign svg { width: 18px; height: 18px; }
@media (max-width: 600px) {
  .campaign-link { gap: 5px 10px; padding: 9px 10px; }
  strong { flex: 1 1 calc(100% - 40px); }
  .next { margin-left: 0; }
}
</style>
