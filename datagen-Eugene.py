# SynthData-Psych
# Cosima Fröhner

import pandas as pd
from pydantic import BaseModel, create_model
from openai import OpenAI
import numpy as np
import os

client = OpenAI(api_key="KEY")

# Read in responses to generic Conscientiousness items form IPIP (Goldberg, 2006) and generic Honesty-Humility from HEXACO (Ashton, 2007)
# from Prolific sample, including demographic data
Input_dataset = pd.read_csv("Data/Prompt_Input.csv")
print(Input_dataset.head())

# Create output folder if not existent
output_dir = "Data/LLM_Responses"
os.makedirs(output_dir, exist_ok=True)

# Split into batches to avoid crash and run each batch manually
num_batches = 20
shuffled = Input_dataset.sample(frac=1, random_state=123).reset_index(drop=True)
batches = np.array_split(shuffled, num_batches)
batch_idx = 1  # pick a batch to run, set to 1, .., num_batches
Input_dataset = batches[batch_idx - 1].reset_index(drop=True)

# Set up response data frame for contextualized Conscientiousness and Honesty-Humility at Work items
results_df = pd.DataFrame(columns=[
    'Participant.id',
    'LLM_C_W_details',
    'LLM_C_W_tasks',
    'LLM_C_W_records',
    'LLM_C_W_responsibility',
    'LLM_C_W_doublecheck',
    'LLM_C_W_standards',
    'LLM_C_W_safety',
    'LLM_C_W_workhours',
    'LLM_C_W_deadlines',
    'LLM_C_W_initiative',
    'LLM_C_W_feedback',
    'LLM_C_W_othersresponsibility',
    'LLM_C_W_organization',
    'LLM_C_W_meetings',
    'LLM_C_W_changes',
    'LLM_C_W_waste',
    'LLM_C_W_credit',
    'LLM_C_W_questions',
    'LLM_C_W_guidelines',
    'LLM_C_W_meetingsskip',
    'LLM_HH_W_promotion',
    'LLM_HH_W_coworkers',
    'LLM_HH_W_responsibility',
    'LLM_HH_W_perspectives',
    'LLM_HH_W_criticism',
    'LLM_HH_W_credit',
    'LLM_HH_W_pressure',
    'LLM_HH_W_opportunity',
    'LLM_HH_W_colleagues',
    'LLM_HH_W_disagreements',
    'LLM_HH_W_respect',
    'LLM_HH_W_integrity',
    'LLM_HH_W_guidance',
    'LLM_HH_W_ownership',
    'LLM_HH_W_recognition',
    'LLM_HH_W_doublecheck',
    'LLM_HH_W_meetings',
    'LLM_HH_W_assistance',
    'LLM_HH_W_attacks',
    'LLM_HH_W_humility'
])

# Loop through one participant per prompt
for i in range(len(Input_dataset)):
    Input = Input_dataset.iloc[i]
    
    prompt = f"""
    Meet this participant: 
    - sex: {Input['sex_cat']};
    - age: {Input['age']}; 
    - education: {Input['educ_cat']};
    - employment status: {Input['employ_cat']};
    - marital status: {Input['marital_cat']};
    - Participant scores on the Big Five Personality test (per trait with a possible minimum of 5 and possible maximum of 100): 
        - Extraversion {Input['E_sum']} 
        - Agreeableness {Input['A_sum']}
        - Conscientiousness {Input['C_sum']}
        - Neuroticism {Input['N_sumscore']}
        - Openness to Experience {Input['O_sum']}

    Imagine you are this participant and infer how this participant would likely respond to each of the new test items below, using a scale from 1 (Disagree) to 5 (Agree).
    Respond with a single number (1–5) per new item, separated by commas.
    Do not include explanations—only the numbers.

    New Test Items for emotional intelligence:
    
    How much do you agree with each statement on a scale from 1 (strongly disagree) to 5 (strongly agree):

    1. Am calm even in tense situations.
    2. Am concerned about others.
    3. Am deeply moved by others' misfortunes.
    4. Am easily moved to tears.
    5. Am not easily disturbed by events.
    6. Am not in touch with my feelings.
    7. Am strongly influenced by the good moods of others.
    8. Am unaffected by other people's happiness.
    9. Am unaffected by the suffering of others.
    10. Am upset by the misfortunes of strangers.
    11. Am usually aware of the way that I'm feeling.
    12. Base my goals in life on inspiration, rather than logic.
    13. Believe emotions give direction to life.
    14. Believe important decisions should be based on logical reasoning.
    15. Believe that criminals should receive help rather than punishment.
    16. Believe the poor deserve our sympathy.
    17. Can't help but look upset when something bad happens.
    18. Dislike being around happy people when I'm feeling sad.
    19. Dislike children's birthday parties.
    20. Don't like to get involved in other people's problems.
    21. Express my affection physically.
    22. Express my happiness in a childlike manner.
    23. Feel little concern for others.
    24. Feel other people's joy.
    25. Feel sympathy for those who are worse off than myself.
    26. Find it difficult showing people that I care about them.
    27. Find it difficult showing people that I'm angry with them.
    28. Find it hard to stay in a bad mood if the people around me are happy.
    29. Get caught up in the excitement when others are celebrating.
    30. Have difficulty showing affection.
    31. Have little sympathy for the unemployed.
    32. Have no sympathy for criminals.
    33. Hug my close friends.
    34. Keep my feelings to myself, regardless of how scared I am.
    35. Keep my feelings to myself, regardless of how unhappy I am.
    36. Keep my happy feelings to myself.
    37. Laugh out loud if something is funny.
    38. Like to watch children open presents.
    39. Listen to my brain rather than my heart.
    40. Listen to my feelings when making important decisions.
    41. Listen to my heart rather than my brain.
    42. Look down on any weakness.
    43. Make decisions based on facts, not feelings.
    44. Notice my emotions.
    45. Often ignore my feelings.
    46. Often stop to analyze how I'm feeling.
    47. Pay a lot of attention to my feelings.
    48. Plan my life based on how I feel.
    49. Plan my life logically.
    50. Rarely analyze my emotions.
    51. Rarely cry during sad movies.
    52. Rarely get caught up in the excitement.
    53. Rarely notice my emotional reactions.
    54. Rarely show my anger.
    55. Rarely think about how I feel.
    56. Remain calm during emergencies.
    57. Shout or scream when I'm angry.
    58. Show my fear.
    59. Show my feelings when I'm happy.
    60. Show my sadness.
    61. Sometimes laugh out loud when reading or watching TV.
    62. Suffer from others' sorrows.
    63. Suspect that my facial expressions give me away when I feel sad.
    64. Sympathize with the homeless.
    65. Think about the causes of my emotions.
    66. Usually end up laughing if the people around me are laughing.
    67. Wish I could more easily show my negative feelings.
    68. Would be upset if I saw an injured animal.


    Predicted Responses (comma-separated):"""

    completion = client.chat.completions.create(
        model="gpt-4o",
        messages=[{"role": "user", "content": prompt}],
        temperature=0,
        frequency_penalty=0,
        max_tokens=200,
        presence_penalty=0,
    )

    # Extract and clean response
    response = completion.choices[0].message.content.strip()
    response = response.replace('\n', ',')
    response = response.replace(',,', ',')

    predicted_responses = [int(x.strip()) for x in response.split(',') if x.strip()]
    print(f"\nRaw response for participant {Input['Participant.id']}:\n{response}")
    
    # Append results and demo variables to DataFrame
    results_df.loc[i] = [
    Input['Participant.id'],
    *predicted_responses
    ]

# Save the results to a CSV file
out_path = os.path.join(output_dir, f"syntheticResponses_batch{batch_idx:02d}.csv")
results_df.to_csv(out_path, index=False)
# results_df.to_csv("syntheticResponses.csv", index=False)
print(f"Results saved for batch {batch_idx}: {out_path}")
