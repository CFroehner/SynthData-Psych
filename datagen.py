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
    Meet this participant, a {Input['Age']}-year-old {Input['Sex']} from {Input['Nationality']} with an annual income of {Input['income']}.
    In terms of personal life, the participant is {Input['relationship_status_word']}.

    The participant provided responses to a Big Five Personality test. The resulting scores are given below.
    Based on the resulting score, infer how the same participant would likely respond to each of the new statements, using a scale from 1 (Disagree) to 5 (Agree).  
    Evaluate each new item individually based on the trait score.
    Respond with a single number (1–5) per new item, separated by commas.
    Do not include explanations—only the numbers.

    Participant score on Conscientiousness: {Input['Gen_C_sumscore']} (with possible minimum 12 and possible maximum 60)

    New Test Items for Conscientiousness at Work (Rate each 1–5):

    1. I am organized and pay attention to details.  
    2. I finish tasks on time.  
    3. I keep accurate records of my work.
    4. I take responsibility for my mistakes.
    5. I double check my work before submitting it.
    6. I set high standards for myself.  
    7. I follow safety protocols.  
    8. I am flexible with my work hours.  
    9. I complete assignments ahead of deadlines.  
    10. I take initiative in learning new skills or procedures.  
    11. I always seek feedback from my supervisors or peers on how to improve my work performance.  
    12. I make sure that others are held accountable for their responsibilities.  
    13. I proactively look for ways to contribute to the organization's goals and objectives.  
    14. I come prepared and well-informed to meetings. 
    15. I am open to changes in working practices or processes.  
    16. I waste time at work.  
    17. I take credit for the accomplishments of others.  
    18. I rarely ask questions when something is unclear.  
    19. I ignore guidelines and policies.  
    20. I skip out on important meetings.

    
    Participant score on Honesty-Humility: {Input['Gen_HH_sumscore']} (with possible minimum 10 and possible maximum 50)

    New Test Items for Honesty-Humility at Work (Rate each 1–5):

    1. I would never tell a lie to get a promotion.
    2. I put the needs of my coworkers before my own.
    3. I try to recognize when I have made a mistake and take responsibility for it.
    4. I strive to understand different perspectives.
    5. I am willing to listen to constructive criticism.
    6. When given credit, I make sure to share it with those who deserve it.
    7. Even when there is pressure from others, I remain honest about my work.
    8. If given the opportunity, I will try to benefit myself over others.
    9. When talking with colleagues, I always aim to stay positive and respectful.
    10. In disagreements, I strive to be open-minded and accept other opinions.
    11. When working in teams, I give everyone equal respect regardless of rank or experience.
    12. Even when facing tough challenges, I maintain my integrity.
    13. When working on tasks that are difficult or unfamiliar, I seek guidance rather than staying quiet.
    14. When in a leadership position, I take ownership of successes and failures.
    15. Rather than seeking recognition or praise for myself, I prefer to focus on team success.
    16. To ensure accurate results, I double check my work before submitting it.
    17. In meetings or presentations, I make sure not to exaggerate facts.
    18. If a colleague is struggling with their work load, I offer assistance if possible.
    19. When someone has done something wrong, I treat them fairly and avoid personal attacks.
    20. At all times, I aim to maintain an attitude of humility.

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
