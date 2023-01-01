--Fisher's Exact Test--

Lady-Tasting-Tea-Problem.R

R.A. Fisher was one of the first to formalize hypothesis testing. The Lady Tasting Tea is one of the most famous examples. The story goes like this.

Muriel Bristol, a colleague of Fisher's, claimed that she could tell if milk was added before or after tea was poured. Fisher was skeptical. He designed an experiment to test this claim. He gave her 4 pairs of cups of tea, 1 with milk poured first, the other after. The order of the two was randomized.

The null hypothesis here is that she was just guessing. Fisher derived the distribution of the number of correct picks on the assumption that the choices were random and independent. As an example, suppose she picked 3 out of 4 correctly. Do we believe she has a special ability based on this?

The basic question we ask is, if the tester is actually guessing, what are the chances that she gets 3 or more correct? Just as we have done before, we can compute a probability under the null hypothesis that she's guessing four of each.