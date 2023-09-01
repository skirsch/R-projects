# R-projects
You are likely here to see my nursing home analysis which is in the directory nursing.

If you are using R studio, just load up the analysis.R file to generate all the data from the original CDC .csv files which I've included here in the data directory.

I recently updated the code to use a 1 week lag on cases after determinging a .99 correlation between cases and deaths. I also tried natural spline interpolation to try to fine tune this, but .99 correlation is as good as it gets so I got lucky with the 1 week lag.

UPenn professor Jeffrey Morris claims I got it wrong in his post 
https://twitter.com/jsm2334/status/1694819462397006029?s=20

But sorry, he's wrong. The more he attacks this, the worse he's going to look because my next step is to get others to validate this. 

I picked week ending 12/6/20 as my reference point for the OR calculation simply because that was the value existing RIGHT BEFORE the vaccines rolled out. That's the point of interest... did the IFR get better or worse AFTER the intervention was rolled out. 

You can see from my twitter post very clearly that the OR was STABLE before the vax rollout. STABLE... FLAT LINED....Then it shot up to an OR of 1.8. That's insane for an entire country. THIS IS A DISASTER.

The OR, after the vaccine rolled out, should have gone from the 1 reference value to 0.1... a 10X reduction. That's what they said. But it almost doubled to 1.8. That's insane.

Eventually, natural immunity finally kicked in and the OR flatlined.

It wasn't the vaccine. The vaccine made things worse.

This isn't a close call.

The 1 week lag should have ONLY been used for the weeks tables, not for the provider and state tables. So the computed fields (IFR, odds, odds ratio, ARR) are inaccurate on these tabs right now (9/1/23)

in the ALL spreadsheet are tabs for Odds ratio and ARR. These are extracted from the week tables for each states and ARE ACCURATE.

I'm on a month long vacation right now but will try to fix the code up to fix these problems. in the meantime, becareful with the data.

The week, odds ratio, and arr tabs in the spreadsheets are all valid.

There are some data quality issues I have to look into, but the signal is super strong here and it's important to let people know this asap which is why I made this public.

Also, you can find states where the vaccine "works" but that's finding data that agrees with your beliefs, not explaining the data that doesn't.

If you'd like to challenge my analysis, please see the Contact me instructions on my pinned tweet on twitter.