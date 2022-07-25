

#-------------------------------------------------------
#estimate the number of episodes
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Pos",agecat="Younger child",tx="hhtx",ahivcat="No"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Neg",agecat="Younger child",tx="hhtx",ahivcat="No"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Pos",agecat="Older child",tx="hhtx",ahivcat="No"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Neg",agecat="Older child",tx="hhtx",ahivcat="No"),ci="normal",cl=0.95)

envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Pos",agecat="Younger child",tx="hhtx",ahivcat="Yes"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Neg",agecat="Younger child",tx="hhtx",ahivcat="Yes"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Pos",agecat="Older child",tx="hhtx",ahivcat="Yes"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Neg",agecat="Older child",tx="hhtx",ahivcat="Yes"),ci="normal",cl=0.95)




