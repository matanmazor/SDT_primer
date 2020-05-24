## The meta-d' model

In the simple SDT model, the same process that is generating decisions is also responsible for generating confidence ratings. This fact makes this simple model unsuitable for the study of metacognition which is assumed to be more implicated in confidence rating than in the decision process itself. 
As a way to dissociate between decision accuracy and confidence reliability, [Brian Maniscalco and Hakwan Lau](https://www.sciencedirect.com/science/article/pii/S1053810011002303) propose to extract two measures from the data: d' and meta-d'. d' is extracted from the decision data alone, ignoring confidence ratings, using the same formula that is used in the traditional model: *d' = z(hit rate)-z(false alarm rate)*. Meta-d' is extracted from the conditional distributions of confidence ratings given specific stimulus and response, by choosing the set of parameters $\theta$ (including meta-d' and meta-criterion) that maximizes the following term:

$L(\theta|data)\propto\prod_{c,s,r}Prob_\theta(Conf_{c}|Stim_s,Resp_r)^{n_{data}(Conf|Stim_s,Resp_r)}$

Put in human language, meta-d' is your best guess of what d' was, if all you know is the conditional distributions of confidence given stimulus and response, and you have no access to the conditional probability of response given stimulus. Changing d' will affect the relative portions of the signal and noise distributions that fall to the right or to the left of the decision criterion. Changing meta-d', on the other hand, will not affect the size of these portions, but will change the shapes they take. In the below widget, d' and the decision criterion are fixed to the values you chose for them above, so the relative portions of the noise and signal distributions that fall to the right or to the left of the criterion are fixed. However, changing meta-d' and the meta-criterion will affect the shape these portions take. 

Notice how in this model the type-2 ROC curve is unaffected by *d'*. Try it!
