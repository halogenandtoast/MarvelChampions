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

Make Me Angry, Synthezoid Strike and Phased Out remain unfinished, as does native scenario/UI integration.
