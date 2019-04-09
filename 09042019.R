Tipo = factor(c(rep('BAP',90),rep('TDZ',90)))
Nivel = factor(rep(c(rep(5,30), rep(1,30),rep(0.1,30)),2))
Auxina = factor(rep(c(rep('NAA',10),rep('IBA',10),rep('2-4D',10)),6))
Bloco = factor(rep(1:10, 18))
y = c(1,1,0,0,1,0,1,0,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,0,1,1,1,0,1,1,0,1,1,1,1,1,
0,0,1,1,1,0,1,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,
1,1,1,1,1,1,0,1,1,1,0,0,1,0,1,1,1,1,1,1)

m = 1
resp = cbind(y, m-y)
model = glm(resp ~ Tipo * Nivel * Auxina + Bloco, family = binomial(link='logit'))
summary(model)

model = glm(resp ~ Bloco, family = binomial(link='logit'))
summary(model)
anova(model, test = 'Chisq')

model = glm(resp ~ Bloco + Nivel, family = binomial(link='logit'))
summary(model)
anova(model, test = 'Chisq')

model = glm(resp ~ Bloco + Tipo, family = binomial(link='logit'))
summary(model)
anova(model, test = 'Chisq')

model = glm(resp ~ Bloco + Tipo + Nivel, family = binomial(link='logit'))
summary(model)
anova(model, test = 'Chisq')

model = glm(resp ~ Bloco + Tipo*Nivel*Auxina, family = binomial(link='logit'))
summary(model)
anova(model, test = 'Chisq')

model = glm(resp ~ Bloco + Tipo*Auxina, family = binomial(link='logit'))
summary(model)
anova(model, test = 'Chisq')

tapply(y , list(Auxina,Tipo), mean)
