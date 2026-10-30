import random
import re


# Der Text als Liste von Wörtern und Satzzeichen
def words(text):
    return re.findall(r"[^\W\d_]+|[.,;:!?]", text)
