# Synthezoid leader reward dependencies

These cards are implemented only as dependencies of the competitive encounter content. The expansion's ordinary player-card content is outside this task.

## Verified events

You're Out of Order! is an Interrupt usable in either identity form, without requiring a friendly leader. It prevents up to three threat from a pending placement on the local main scheme, whether that placement came from an ability, a villain or acceleration. The replacement preserves the original placement's timing and does not open another would-place-threat window.

She-Hulk Smash requires a friendly She-Hulk leader and makes her attack the local enemy leader. Its stun bonus checks the resources actually paid and applies only when at least one physical resource was spent. The shared leader-attack path retains boost handling and attack responses.

Calculated Risk searches at most the top five cards of the friendly leader's encounter deck. Only the acting player sees and chooses among those cards. The selected physical card is dealt facedown to the opposing team's first player, without being revealed, and retains its original deck home. The entire searched deck is then shuffled, as required when any portion of a deck is searched (Rules Reference v1.8, Search, p.39).

Validation: eleven new behavior cases pass with the full Civil War and coverage checks (1,136 examples, zero failures). They cover paid-resource conditions, wrong-leader restrictions, all three tested threat sources, full and partial prevention, both search sizes, selection bounds, recipient and independent deck contents. A recent rulings search found no Calculated Risk exception to the standard search-and-shuffle rule.

## Resource dependencies

Legal Counsel generates two wild resources in either identity form. Its optional Hero Response moves one threat from the spender's local main scheme to the friendly leader's main scheme. The response is offered only when threat can move, and the spender may decline it without undoing payment. Its move uses the normal removal and placement rules.

Android Assistance also generates two wild resources in either form. Its optional Hero Interrupt flips the friendly Vision leader's existing mass-form attachment, preserving the attachment's identity and host. It is not offered without the relevant friendly Vision and form. A dedicated before-spending-effect hook uses the actual spender, including contributed and held cards, and runs before after-spending responses and before the paid card or ability resolves. The existing Cyclops spending interrupt continues through its own hook.

Validation: twelve new resource behavior cases pass with Civil War, coverage, Cyclops, Cyclops mechanics and actual-resource checks (1,216 examples, zero failures). These cover optional responses, identity-form limits, both mass forms, ability payments, payments without a friendly leader, and Android Assistance resolving before Legal Counsel regardless of spend order. The English label JSON parses successfully.

All eight reward dependencies are implemented. Native scenario/UI integration remains unfinished.


## Synthezoid Strike

The starting mass form determines the event's branch. Dense grants +1 ATK and piercing for this attack. Intangible is flipped to Dense before the attack, so Vision uses Dense's normal ATK modifier but does not also receive the event's Dense-branch bonus or piercing.

An explicit character attack can now carry its own signed ATK adjustment and granted keywords through the ordinary villain attack pipeline. The adjustment is part of that attack's state from its declaration, survives rebuilding the attack at its beginning, and is cleared with the attack. A stun replacement creates no attack to hold the adjustment, so it cannot leak into a later activation. No lasting modifier is placed on Vision.

Validation: six new Synthezoid Strike behavior cases pass with the full Civil War and coverage checks (1,154 examples, zero failures). They cover both starting forms, both tough states, cleared bonuses and piercing on subsequent attacks, and a stun-replaced attack.


## Phased Out

Phased Out costs zero and discards a legally discardable attachment on the active enemy leader with the text Hero Action or Hero Response. The same matcher governs both play eligibility and the target question, so the event is not offered without an effect it can resolve. Hero Interrupt and Forced Response text do not qualify, and an eligible attachment on the friendly leader's board does not qualify.

The existing Sunfire metadata identifies the printed text, including conditional abilities that are currently unavailable. Attachments protected from discard or leaving play are excluded, as are permanent attachments. The player chooses among eligible targets. The event needs no friendly leader and can resolve in cooperative play.

Validation: ten Phased Out checks and two new Moon Knight erratum checks pass with Civil War, coverage and Cyclops aspect tests (1,234 examples, zero failures). This uses the official Phased Out erratum (RR1.8 FAQ, p.70), replacing Hero Interrupt with Hero Action. The viewer text and its generator include that correction.


## Make Me Angry

Make Me Angry attaches to the player's friendly She-Hulk leader. Its target validation rejects a different leader or one that cannot receive player attachments. Its forced response distinguishes attacks by heroes and leaders from attacks by allies or minions, and makes She-Hulk attack the original attacker through the native attack pipeline.

The counterattack is deferred until the original activation and its responses finish. The upgrade records whether its counterattack actually reaches its after-attack window, then discards after that attack and its responses finish. Under the RR1.8 Then and Stun entries, a stun replacement does not make the attack, so the post-Then discard is not performed; the card remains available for a later trigger. Pending and completion state are saved with the upgrade.

Validation: all seven new Make Me Angry behavior cases pass, including hero and leader attackers, ally exclusion, attachment restrictions, and stun followed by a later successful counterattack. The full Civil War and coverage regression run passes (1,173 examples, zero failures). Native Synthezoid scenario/UI integration remains unfinished.
