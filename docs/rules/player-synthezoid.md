# Synthezoid leader reward dependencies

These cards are implemented only as dependencies of the competitive encounter content. The expansion's ordinary player-card content is outside this task.

## Events implemented in this batch

You're Out of Order! is an Interrupt usable in either identity form, without requiring a friendly leader. It prevents up to three threat from a pending placement on the local main scheme, whether that placement came from an ability, a villain or acceleration. The replacement preserves the original placement's timing and does not open another would-place-threat window.

She-Hulk Smash requires a friendly She-Hulk leader and makes her attack the local enemy leader. Its stun bonus checks the resources actually paid and applies only when at least one physical resource was spent. The shared leader-attack path retains boost handling and attack responses.

Calculated Risk searches at most the top five cards of the friendly leader's encounter deck. Only the acting player sees and chooses among those cards. The selected physical card is dealt facedown to the opposing team's first player, without being revealed, and retains its original deck home. The entire searched deck is then shuffled, as required when any portion of a deck is searched (Rules Reference v1.8, Search, p.39).

Validation: eleven new behavior cases pass with the full Civil War and coverage checks (1,136 examples, zero failures). They cover paid-resource conditions, wrong-leader restrictions, all three tested threat sources, full and partial prevention, both search sizes, selection bounds, recipient and independent deck contents. A recent rulings search found no Calculated Risk exception to the standard search-and-shuffle rule.

Make Me Angry, Legal Counsel, Synthezoid Strike, Phased Out and Android Assistance remain unfinished, as does native scenario/UI integration.
