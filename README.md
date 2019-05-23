# IncrementalTextRank
Automatic Keyword/Keyphrase Extraction from Text Streams

# About this Programming Code

Four versions available:

- One with Landmark type of stream window, for a stream of several documents at a time
- One with Landmark type of stream window, for a single document incremental stream
- One with Sliding window, for a stream of several documents at a time
- One with Sliding window, for a single document incremental stream

# Abstract

Text Mining and NLP techniques are a hot topic nowadays. Researchers thrive to develop new and faster algorithms to cope with larger amounts of data. Particularly, text data analysis has been increasing in interest due to the growth of social networks media. Given this, the development of new algorithms and/or the upgrade of existing ones is now a crucial task to deal with text mining problems under this new scenario. In this paper, we present an update to TextRank, a well-known implementation used to do automatic keyword extraction from text, adapted to deal with streams of text. In addition, we present results for this implementation and compare them with the batch version. Major improvements are lowest computation times for the processing of the same text data, in a streaming environment, both in sliding window and incremental setups. The speedups obtained in the experimental results are significant. Therefore the approach was considered valid and useful to the research community.

# Cite Paper Publication

@inproceedings{sarmento2018incremental,
  title={Incremental TextRank - Automatic Keyword Extraction for Text Streams},
  author={Sarmento, Rui Portocarrero and Cordeiro, M{\'a}rio and Brazdil, Pavel and Gama, Jo{\~a}o},
  booktitle={20th International Conference on Enterprise Information Systems},
  volume={1},
  number={SciTePress},
  pages={363--370},
  year={2018}
}
