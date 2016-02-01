from os import path
from wordcloud import WordCloud
from wordcloud import STOPWORDS
import matplotlib.pyplot as plt
import re
filename="/Users/chunmeiGao/Documents/Dataincubator/emailsubject.txt"

# Read the whole text.
text = open(filename).read()
print text
text=re.sub('Re:', '', text)
text=re.sub('RE:', '', text)
text=re.sub('FW:', '', text)
text=re.sub('Fwd:', '', text)
text=re.sub('Enron', '', text)
more_stopwords = {'X', 'Re', 'Fwd','ENRON','NA','FW'}
STOPWORDS = STOPWORDS.union(more_stopwords)
# Generate a word cloud image
wordcloud = WordCloud(stopwords=STOPWORDS).generate(text)

# Display the generated image:
# the matplotlib way:

plt.imshow(wordcloud)
plt.axis("off")

# take relative word frequencies into account, lower max_font_size
wordcloud = WordCloud(max_font_size=40, relative_scaling=.5).generate(text)
plt.figure()
plt.imshow(wordcloud)
plt.axis("off")
plt.show()

__author__ = 'chunmeiGao'
