#%%
# NLP ANALYSIS
import pandas as pd
from IPython.display import clear_output

## Clean text
import re
from cleantext import clean

## NLP
import spacy
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import nltk

nltk.download('vader_lexicon')
nlp = spacy.load("en_core_web_sm")
stop_words = set(stopwords.words('english'))

#%%
dados = pd.read_csv('reddit_posts.csv')
dados = dados.drop(['Unnamed: 0'], axis = 1)
dados = dados[dados.body != '[removed]']

#%%
# CLEAN TEXT
def get_clean_text(text):
    
    ## Initial clean
    text = clean(text, 
                 lower = True, no_line_breaks = True, no_urls = True, no_emails = True,
                 no_phone_numbers = True, no_numbers = True, no_digits = True, no_currency_symbols = True,
                 no_punct = True, replace_with_punct = " ", replace_with_url="", replace_with_email="",
                 replace_with_phone_number = "", replace_with_number = "", replace_with_digit = "",
                 replace_with_currency_symbol = "")

    doc = nlp(text)

    ## lemmatize
    text_clean = [token.lemma_ if token.pos_ in ('VERB', 'AUX') else token.orth_ for token in doc]
    ## remove stop words
    text_clean = [word for word in text_clean if word not in stop_words]
    ## Remove withespaces
    text_clean = [word for word in text_clean if word and word.strip()]
    ## Remove emojis and special caracters
    RE_EMOJI = re.compile('[\U00010000-\U0010ffff]', flags=re.UNICODE)
    text_clean = [RE_EMOJI.sub(r'', word) for word in text_clean if word not in ['>']]

    return ' '.join(text_clean)

dados['clean_text'] = dados['body'].apply(lambda x: get_clean_text(x))

#%%
# GET POLARITY SCORES
dados_nlp = dados.copy()
dados_nlp = dados_nlp.reset_index()

sent_neg = []
sent_pos = []
sent_neu = []
sent_compound = []
for index, row in dados_nlp.iterrows():
    if index % 100 == 0:
        clear_output()
        print(round(index / dados.shape[0] * 100, 2))
    
    text_polarity = SentimentIntensityAnalyzer().polarity_scores(row['clean_text'])
    
    sent_neg.append(text_polarity['neg'])
    sent_pos.append(text_polarity['pos'])
    sent_neu.append(text_polarity['neu'])
    sent_compound.append(text_polarity['compound'])

#%%
# FINAL DATAFRAME
dados_nlp['sent_neg'] = sent_neg
dados_nlp['sent_pos'] = sent_pos
dados_nlp['sent_neu'] = sent_neu
dados_nlp['sent_compound'] = sent_compound

dados_nlp.to_csv('reddit_sent_analysis.csv', index = False)

#%%
# WORDCLOUD
dados = pd.read_csv('reddit_sent_analysis.csv')
dados = dados[dados.body != '[removed]']

## https://www.geeksforgeeks.org/generating-word-cloud-python/
from wordcloud import WordCloud, STOPWORDS
import matplotlib.pyplot as plt

comment_words = ''
stopwords = set(STOPWORDS)
 
# iterate through the csv file
for val in dados.clean_text:
     
    # typecaste each val to string
    val = str(val)
 
    # split the value
    tokens = val.split()
     
    # Converts each token into lowercase
    for i in range(len(tokens)):
        tokens[i] = tokens[i].lower()
     
    comment_words += " ".join(tokens)+" "
 
wordcloud = WordCloud(width = 800, height = 800,
                background_color ='white',
                stopwords = stopwords,
                min_font_size = 10).generate(comment_words)

# plot the WordCloud image                      
plt.figure(figsize = (8, 8), facecolor = None)
plt.imshow(wordcloud)
plt.axis("off")
plt.tight_layout(pad = 0)
plt.title('Reddit Posts WordCloud', fontsize=40)

plt.savefig('foo.png', bbox_inches='tight')