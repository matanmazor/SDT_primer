# Understanding meta-d'



The purpose of this page is to give a visual intuition to Maniscalco and Lau's meta-d' measure. But before doing this, I'll shortly introduce the traditional "first order" model.

## First-order SDT

We will model a perceptual confidence experiment where participants rate their confidence in whether a signal was present ('yes') or absent ('no'). In our example, confidence is reported on a scale of 1-6. The same Signal Detection Theory (SDT) model that is traditionally used to model decisions in such tasks can be naturally extended to model confidence ratings as well. This is done by including a set of *confidence criteria* around the decision criterion. These criteria carve the evidence axis into a set of bins, and bins farther away from the decision criterion correspond to higher decision confidence. In the bellow application I assumed for simplicity that the distance between criteria is fixed, but this is not a necessary assumption to make. For the purpose of this demonstration I will assume that the signal and noise distributions differ only by their means and have the same variance. <br>
At the top right of the screen you'll find the type-1 and type-2 ROC curves for your choice of parameter values. Notice that both are affected by *d'*.

