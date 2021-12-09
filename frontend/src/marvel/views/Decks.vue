<template>
  <div id="decks">
    <div>
      <h2>New Deck</h2>
      <div class="new-deck">
        <img v-if="identity" class="portrait" :src="`/img/marvel/cards/${identity.replace('c', '')}.jpg`" />
        <div class="fields">
          <input
            type="url"
            v-model="deck"
            @change="loadDeck"
            @paste.prevent="pasteDeck($event)"
            placeholder="MarvelDB deck url"
          />
          <input v-if="identity" v-model="deckName" />
          <button :disabled="!identity" @click.prevent="createDeck">Create</button>
        </div>
      </div>
    </div>
    <h2>Existing Decks</h2>
    <div v-for="deck in decks" :key="deck.id" class="deck">
      <span>{{deck.name}}</span>
      <div class="deck-delete">
        <a href="#delete" @click.prevent="deleteId = deck.id"><font-awesome-icon icon="trash" /></a>
      </div>
    </div>

    <Prompt
      v-if="deleteId"
      prompt="Are you sure you want to delete this deck?"
      :yes="deleteDeckEvent"
      :no="() => deleteId = null"
    />
  </div>
</template>

<script lang="ts">
import { defineComponent, ref } from 'vue';
import * as Marvel from '@/marvel/types/Deck';
import Prompt from '@/components/Prompt.vue';
import { fetchDecks, newDeck, deleteDeck } from '@/marvel/api';
export default defineComponent({
  components: { Prompt },
  setup() {
    const ready = ref(false)
    const decks = ref<Marvel.Deck[]>([])
    const deck = ref<string | null>(null)
    const identity = ref<string | null>(null)
    const deckId = ref<string | null>(null)
    const deckName = ref<string | null>(null)
    const deckUrl = ref<string | null>(null)
    const deleteId = ref<string | null>(null)
    async function deleteDeckEvent() {
      const { value } = deleteId
      if (value) {
        deleteDeck(value).then(() => {
          decks.value = decks.value.filter((deck) => deck.id !== value);
          deleteId.value = null;
        });
      }
    }
    fetchDecks().then((response) => {
      decks.value = response
      ready.value = true
    })
    function loadDeck() {
      if (!deck.value) {
        return;
      }
      const matches = deck.value.match(/\/(deck(list)?)(\/view)?\/([^/]+)/);
      if (matches) {
        deckUrl.value = `https://marvelcdb.com/api/public/${matches[1]}/${matches[4]}.json`
        fetch(deckUrl.value)
          .then((response) => response.json(), () => {
            identity.value = null;
            deckId.value = null
            deckName.value = null
            deckUrl.value = null;
          })
          .then((data) => {
            console.log(data)
            identity.value = data.investigator_code
            deckId.value = matches[4]
            deckName.value = data.name
          })
      } else {
        identity.value = null;
        deckId.value = null
        deckName.value = null
        deckUrl.value = null;
      }
    }
    function pasteDeck(evt: ClipboardEvent) {
      if (evt.clipboardData) {
        deck.value = evt.clipboardData.getData('text');
        loadDeck();
      }
    }
    async function createDeck() {
      if (deckId.value && deckName.value && deckUrl.value) {
        newDeck(deckId.value, deckName.value, deckUrl.value).then((deck) => decks.value.push(deck));
        deckId.value = null;
        deckName.value = null;
        deckUrl.value = null;
        identity.value = null;
        deck.value = null;
      }
    }
    return { pasteDeck, createDeck, deleteDeckEvent, deleteId, deck, decks, loadDeck, identity, deckName }
  }
})
</script>
