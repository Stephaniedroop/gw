# Rating guidelines

## The task

I need some annotations done of some free text utterances. You are a clever
and reliable but basically normal human annotator. You will be given a series
of explanations in natural language. They are explanations for why a character
performed an outcome (i.e. went a certain path to get a certain food). The set
of outcomes they perform is four possible ones: long or short path to hotdog
or pizza. You don't need to know what the character did; you just need to
annotate the participant's explanation. For each one you need to output which
of a set of factors is being cited in the explanation as the best reason for
the behavior.

## Use fuzzy LLM language intuitions rather than writing me a rule-based script

I don't want you to do the rating in a rule-based way. I need flexible
interpretation and perception of implicit meaning rather than pattern-matching.
I want to harness the full, fuzzy language expertise of the LLM itself. For
this reason I don't want a reproducible python script. I will drop the free
text utterances for rating straight into the chat once you have understood the
task and said you are ready.

## Understanding the \_f and \_p suffixes

Some variables have two versions, marked \_f (food) and \_p (path). These do
not describe two different kinds of the variable — they describe which outcome
the variable is being recruited to explain. For example, a momentary food
craving (Pu) might explain what someone chose to eat (Pu_f) or it might
explain which path they took (Pu_p). When you see a \_f or \_p variable, ask: is this
explanation about why they got the food they got, or about why they went the
way they went?

## Choosing the best label

Sometimes the utterance will seem to contain several categories; choose whichever
is the main or most proximate reason cited. Sometimes the utterance will not
reference the state the variable took (e.g. which food was preferred, or which
direction). In these cases use the variable name without a 0 or 1. 'Unclear'
is a last resort for utterances that contain no real causal explanation —
fragments or re-descriptions of the behavior itself. Some labels may be rare
and it is fine if they have no exemplars.

## Set of factors

P=0: stable general liking for pizza or disliking of hotdog
P=1: stable general liking for hotdog or disliking of pizza
P: stable preference mentioned but which food not clear

K=0: generally does not know the area
K=1: generally knows the area

C=0: generally lazy or unenergetic
C=1: generally sporty or active

S=0: starts near the pizza
S=1: starts near the hotdog
S: starting location matters but which food is unclear

Pu_f=0: right now wants pizza or does not want hotdog [explains food choice]
Pu_f=1: right now wants hotdog or does not want pizza [explains food choice]
Pu_f: momentary food preference explains food choice but direction unclear

Pu_p=0: momentary preference was too weak to make him go straight to it or could be deferred to justify
going further [explains long path or not taking short path]
Pu_p=1: momentary preference was strong enough to make him take short path or go straight to it
[explains short path or not taking long path]

Ku_f=0: right now does not know where the food stands are [explains food
choice, e.g. defaulted to nearest]
Ku_f=1: right now knows where a specific food stand is [explains food choice]

Ku_p=0: right now does not know the route, promotes long path [explains path taken]
Ku_p=1: right now knows the route or a shortcut, promotes short path [explains path taken]

Cu=0: right now feeling lazy or low energy
Cu=1: right now feeling energetic or sporty

Su=0: a situational factor that favoured pizza or impeded access to hotdog
Su=1: a situational factor that favoured hotdog or impeded access to pizza
Su: situational factor explains outcome but direction unclear

br_f=0: any other unmodelled reason that would lead to getting pizza or not
getting hotdog
br_f=1: any other unmodelled reason that would lead to getting hotdog or not
getting pizza

br_p=0: any other unmodelled reason that would lead to taking the short path
or not the long path
br_p=1: any other unmodelled reason that would lead to taking the long path
or not the short path

Unclear: a nonsensical fragment or re-description of the behavior with no
causal explanation

## Examples (not exhaustive or definitive)

P=0: 'his favorite food is pizza', 'he likes pizza', 'he doesn't like hotdogs'
P=1: 'his favorite food is hotdogs', 'he likes hotdog', 'he doesn't like pizza'
P: 'it was his favorite'

K=0: 'he didn't know the way', 'he doesn't know the area', 'he had never been there'
K=1: 'he knows there is a hotdog stand round the corner', 'he knows the way'

C=0: 'he is lazy', 'he never wants to go far'
C=1: 'he is sporty', 'he loves being active'

S=0: 'he was near the pizza'
S=1: 'he was near the hotdog'
S: 'he was near'

Pu_f=0: 'he was in the mood for pizza', 'he didn't fancy hotdog'
Pu_f=1: 'he just fancied a hotdog', 'he wanted hotdog'
Pu_f: 'he wanted something different from usual'

Pu_p=0: 'his craving for it made the long walk worth it',
Pu_p=1: 'he wanted it so much he went straight to it',

Ku_f=0: 'he didn't know if there was a hotdog stand nearby',
'he couldn't remember where the stands were'
Ku_f=1: 'he remembered seeing a hotdog stand there',
'he had just been told where the stand was'

Ku_p=0: 'he forgot which way to go', 'he got confused about the route'
Ku_p=1: 'he knew a shortcut', 'he had just been given directions'

Cu=0: 'he was feeling tired', 'he couldn't be bothered', 'he didn't care what he ate'
Cu=1: 'he needed exercise', 'he wanted to work up an appetite'

Su=0: 'he could see the pizza', 'the path was clear',
'the pizza stand was right on his route'
Su=1: 'he could see the hotdog',
'there was a roadblock so he had to go the long way round',
'the hotdog was only reachable from the other side'
Su: 'it was the nearest food he could see'

br_f=0: 'the hotdog queue was enormous', 'the hotdogs looked bad today'
br_f=1: 'the pizza looked stale', 'a friend recommended the hotdog'

br_p=0: 'it was raining so he took the quickest route',
'he was on his phone and went the nearest way'
br_p=1: 'he had time to kill', 'he ran into a friend and they walked together'

Unclear: 'he went the long way', 'the long way round the corner'
