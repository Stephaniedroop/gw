# Rating guidelines

## The task

I need some annotations done of some free text utterances. You are a clever and reliable but basically normal human annotater. You will be given a series of explanations in natural language. They are explanations for why a character performed an outcome (ie. went a certain path to get a certain food). The set of outcomes they perform is four possible ones: long or short path to hotdog or pizza. You don't need to know what the character did; you just need to annotate the participant's explanation. For each one you need to output which of a set of factors is being cited in the explanation as the best reason for the behavior.

## Use fuzzy LLM language intuitions rather than writing me a rule-based script

I don't want you to do the rating in a rule-based way by making a reproducible python script. I don't need a rule-based system but rather flexible interpretation and perception of implicit meaning rather than pattern-matching. I actually want to harness the full, fuzzy, language expertise of the LLM itself. For this reason I don't want to use Claude co-work because that returns a rule-based python script. So I will drop the free text utterances for rating straight into the chat when have understood the task and say you are ready.

## The factors to choose from

The factors are: P, K, C, S Pu, Ku, Cu, Su. There are four general variables (P, K, C, S) and each of those has a 'noise' factor which is like an unobserved or unmodelled reason why the corresponding general variable works or doesn't work right now. Each can take 0 or 1. Below is the definitions of the set of factors and then some examples of each. However, you should NOT make rules to match these and only these exactly. These examples are only guidelines. You should use your interpretation and all your stored knowledge of implied and tacit meanings to choose the best category for each text utterance. Sometimes the utterance will seem to contain several categories but you need to choose which is being cited as the main one, or the 'real' reason for the behavior.

Some factors (Cu, Su, br) are split out between Path and Food outcomes. These are marked "\_p" for Path and "\_f" for Food. Sometimes the distinction is ambiguous in practice: for example an utterance "He couldn't be bothered" might not cleanly be either Cu_p or Cu_f; in this case it is ok to randomly pick either of the labels that fit.

Sometimes the utterance will not reference the state the variable took. In these cases it is ok to put just the variable name. For example "This was the closest food he could see" = put Su_f, or "It was his favorite food" = put P. If these variables on their own are not in the list of ones to choose from then it's ok to add it.

The category 'Unclear' is for any which genuinely dont seem to fit any of the factors or are nonsense. It is a last resort. Some labels may be rare and it is ok if they don't end up with any exemplars.

Set of factors:
P=0: general stable liking for pizza or not hotdog
P=1: general stable liking for hotdog or not pizza
P: favorite but state not mentioned
K=0: generally does not know the area
K=1: generally knows the area
C=0: generally is lazy
C=1: generally is sporty
S=0: starts near the pizza
S=1: starts near the hotdog
S: location important but we don't know which
Pu=0: right now wants a pizza or right now does not want a hotdog
Pu=1: right now wants a hotdog or right now does not want a pizza
Ku=0: right now doesn't know the area
Ku=1: right now knows the area
Cu_f=0: right now is feeling lazy (with an emphasis on not caring what they eat)
Cu_f=1: right now is feeling sporty (with an emphasis on being motivated to seek out the preferred food even if it takes effort)
Cu_p=0: right now is feeling lazy (with an emphasis on not wanting to make any effort)
Cu_p=1: right now is feeling sporty (with an emphasis on wanting to move and get exercise)
Su_f=0: right now they can see the pizza or they cannot see the hotdog
Su_f=1: right now they can see the hotdog or they cannot see the pizza
Su_f: [if the state they cannot see is not discernable from the utterance]
Su_p=0: something that would let them physically access the pizza or not access the hotdog stand
Su_p=1: something that would let them physically access the hotdog or not access the pizza stand
br_p=0: any other reason not modeled that would make them take the short path or not take the long path
br_p=1: any other reason not modeled that would make them take the long path or not take the short path
br_f=0: any other reason not modeled that would make them get pizza or not get hotdog
br_f=1: any other reason not modeled that would make them get hotdog or not get pizza
Unclear: a nonsensical fragment or description of the situation that does not contain a real explanation, cause or reason

Examples, not exhaustive or definitive:
P=0: 'his favorite food is pizza', 'he likes pizza', 'he doesnt like hotdogs'
P=1: 'his favorite food is hotdogs', 'he likes hotdog', 'he doesnt like pizza'
P: 'it was his favorite'
K=0: 'he didnt know the way', 'he doesnt know the area', 'he had never been there'
K=1: 'he knows there is a hotdog stand round the corner', 'he knows where to find the food', 'he knows the way'
C=0: 'he is lazy', 'he never wants to go far'
C=1: 'he is sporty', 'he loves being active'
S=0: 'he was near the pizza'
S=1: 'he was near the hotdog'
S: 'he was near'
Pu=0: 'he was in the mood for pizza', 'he didnt fancy hotdog this minute'
Pu=1: 'just fancied a hotdog', 'he wants hotdog'
Pu: wanted a different food from normal but unknown which one is normal
Ku=0: 'he forgot the layout'
Ku=1: 'he had just been given directions'
Cu_f=0: 'he didnt care what he ate'
Cu_f=1: 'he wanted to work up an appetite for his hotdog'
Cu_p=0: 'he was feeling tired'
Cu_p=1: 'he needed exercise'
Su_f=0: 'he couldn't see the hotdog', 'he could see the pizza'
Su_f=1: 'he could see the hotdog', 'he couldn't see the pizza'
Su_f: 'it was the nearest food he could see'
Su_p=0: 'the path was clear', 'the pizza stand was right on his route'
Su_p=1: 'there was a roadblock so he had to go the long way round', 'the hotdog was only reachable from the other side'
br_p=0: 'it was raining', 'he was on his phone so went the nearest way'
br_p=1: 'he had time to kill', 'he ran into a friend and they walked together'
br_f=0: 'the hotdog queue was enormous', 'the hotdogs looked bad today'
br_f=1: 'the pizza looked stale', 'a friend recommended the hotdog'
Unclear: 'he went the long way', 'the long way round the corner'
