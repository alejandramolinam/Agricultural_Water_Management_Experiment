from otree.api import *


class C(BaseConstants):
    NAME_IN_URL = 'survey_final'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    causa_injusticia=models.IntegerField(min=1,max=5)
    causa_autoridad=models.IntegerField(min=1,max=5)
    causa_necesidad=models.IntegerField(min=1,max=5)
    causa_otros=models.IntegerField(min=1,max=5)
    causa_multa=models.IntegerField(min=1,max=5)
    post_beliefs_menos = models.IntegerField(
        label = '¿Cuántas de ellas cree que decidieron usar <b>menos de las 12 horas</b> asignadas?',
        min = 0,
        max = 10
    )
    post_beliefs_igual = models.IntegerField(
        label = '¿Cuántas de ellas cree que decidieron usar <b>solo las 12 horas</b> asignadas?',
        min = 0,
        max = 10
    )
    post_beliefs_mas = models.IntegerField(
        label = '¿Cuántas de ellas cree que decidieron usar <b>más de las 12 horas</b> asignadas?',
        min = 0,
        max = 10
    )
    pregunta_abierta = models.StringField(blank=True)


# FUNCTIONS
# PAGES
class Motivos(Page):
    form_model = 'player'
    form_fields = ['causa_injusticia','causa_autoridad','causa_necesidad','causa_otros','causa_multa']


class Elicit_beliefs(Page):
    form_model = 'player'
    #form_fields = ['post_beliefs_menos', 'post_beliefs_igual', 'post_beliefs_mas']
    form_fields = ['post_beliefs_mas']

    #@staticmethod
    #def error_message(player, values):
    #    tot = values['post_beliefs_menos'] + values['post_beliefs_igual'] + values['post_beliefs_mas']
    #    if tot != 10:
    #        return 'Debe repartir las 10 personas entre los 3 grupos'


class Pregunta_abierta(Page):
    form_model='player'
    form_fields =['pregunta_abierta']


class Final(Page):
    form_model = 'player'

page_sequence = [Motivos, Elicit_beliefs, Pregunta_abierta, Final]
