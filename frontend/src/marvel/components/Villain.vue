<script lang="ts" setup>
import { defineProps, defineEmits, computed } from 'vue'
import { Game } from '@/marvel/types/Game'
import { Villain } from '@/marvel/types/Villain'
import * as MarvelGame from '@/marvel/types/Game'
import Card from '@/marvel/components/Card.vue'
import Attachment from '@/marvel/components/Attachment.vue'
import Upgrade from '@/marvel/components/Upgrade.vue'
import AbilityButton from '@/marvel/components/AbilityButton.vue'

const props = defineProps<{
  game: Game
  identityId: string
  villain: Villain
}>()

const emit = defineEmits<{
  (e: 'choose', value: number): void
}>()

const card = computed(() => ({ cardId: props.villain.contents.villainId, cardDef: props.villain.contents.villainCardDef }))

const choices = computed(() => MarvelGame.choices(props.game, props.identityId))

const activeAbility = computed(() => {
  return choices.
    value.
    findIndex((choice) => {
      if (choice.tag !== 'TargetLabel') {
        return false
      }

      const { contents } = choice.target
        if (typeof contents === "string") {
          return contents == props.villain.contents.villainId
        }

        switch (contents.tag) {
          case 'EnemyVillainId':
            return contents.contents === props.villain.contents.villainId
          default:
            return false
        }

    })
})

const attachments = computed(() => props.villain.contents.villainAttachments.map((attachmentId) => props.game.attachments[attachmentId]))

const upgrades = computed(() => props.villain.contents.villainUpgrades.map((villainId) => props.game.upgrades[villainId]))

const abilities = computed(() => {
  return choices.value.reduce<number[]>((acc, v, i) => {
    if (v.tag === 'UseAbility' && v.contents.abilitySource.contents == props.villain.contents.villainId) {
      return [...acc, i]
    }
    return acc
  }, [])
})
</script>

<template>
  <div class="villain">
    <Card :card="card" :game="game" :identityId="identityId" @choose="emit('choose', $event)" :class="{ active: activeAbility !== -1 }" @click="emit('choose', activeAbility)"/>
    <div class="hp">{{villain.contents.villainHp}}</div>
    <div v-if="villain.contents.villainStunned">Stunned</div>
    <div v-if="villain.contents.villainConfused">Confused</div>
    <div v-if="villain.contents.villainTough">Tough</div>
    <Attachment
      v-for="attachment in attachments"
      :key="attachment.contents.attachmentId"
      :attachment="attachment"
      :game="game"
      :identityId="identityId"
      @choose="emit('choose', $event)"
    />
    <Upgrade
      v-for="upgrade in upgrades"
      :key="upgrade.contents.upgradeId"
      :upgrade="upgrade"
      :game="game"
      :identityId="identityId"
      class="attached"
      @choose="emit('choose', $event)"
    />
    <AbilityButton
          v-for="ability in abilities"
          :key="ability"
          :ability="choices[ability]"
          @click="emit('choose', ability)"
          />
  </div>
</template>

<style scoped lang="scss">
.villain {
  display: inline-block;
}

.hp {
  font-family: "Exo2";
  font-weight: bold;
  font-size: 1.2em;
  background: #8E1C21;
  display: inline-block;
  padding: 0 10px;
  border: 5px solid #383839;
  color: white;
  border-radius: 10px;
}
</style>
