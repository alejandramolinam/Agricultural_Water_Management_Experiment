from otree.api import *


class C(BaseConstants):
    NAME_IN_URL = 'survey_inicial'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1
    ESTUDIANTES = 1

class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    edad = models.IntegerField(label='¿En que año nació?', min=1900, max=2006)
    genero = models.StringField(
        choices=[['Masculino', 'Masculino'], ['Femenino', 'Femenino'],['Otro','Otro']],
        label='¿Cuál es su género?',
        widget=widgets.RadioSelect
    )
    escolaridad = models.IntegerField(
        choices=[[1,'Sin estudios formales'],
                 [2,'Educación básica o primaria completa'],
                 [3,'Educación media o secundaria completa'],
                 [4,'Universitaria o técnico superior completa'],
                 [5,'Postgrado']],
        label='¿Cuál es el último nivel de escolaridad que terminó?',
        widget=widgets.RadioSelect
    )
    conocimiento_derechos = models.IntegerField(
        choices=[[1, 'Sí'], [0, 'No']],
        label='¿Está usted familiarizado con el funcionamiento de los derechos de agua en Chile?',
        widget=widgets.RadioSelect,
        blank=True
    )
    derecho = models.IntegerField(
        choices=[[1,'Sí'],[0,'No']],
        label='¿Es usted o un familiar directo propietario de un terreno con derecho a usar agua en un canal de regadío?',
        widget=widgets.RadioSelect,
        blank=True
    )
    hectareas = models.IntegerField(
        label='¿Cuántas hectáreas de cultivo necesita regar (aproximadamente)?',
        min=0,
        initial=0,
        blank=True
    )

    pre_beliefs_menos = models.IntegerField(
        label = '¿Cuántas de ellas cree que van a decidir usar <b>menos de las 12 horas</b> asignadas?',
        min = 0,
        max = 10
    )
    pre_beliefs_igual = models.IntegerField(
        label = '¿Cuántas de ellas cree que van a decidir usar <b>solo las 12 horas</b> asignadas?',
        min = 0,
        max = 10
    )
    pre_beliefs_mas = models.IntegerField(
        label = '¿Cuántas de ellas cree que van a decidir usar <b>más de las 12 horas</b> asignadas?',
        min = 0,
        max = 10
    )

    risk_aversion = models.IntegerField(
        choices=[[1, '100% de probabilidad de ganar $720'],
                 [2, '50% de probabilidad de ganar $1080 y 50% de ganar $540'],
                 [3, '50% de probabilidad de ganar $1440 y 50% de ganar $360'],
                 [4, '50% de probabilidad de ganar $1800 y 50% de ganar $180'],
                 [5, '50% de probabilidad de ganar $2160 y 50% de ganar $0']],
        label='Entre las siguientes loterías ¿cuál prefiere jugar?',
        widget=widgets.RadioSelect
    )

# FUNCTIONS
# PAGES
class Bienvenida(Page):
    pass

class Demographics(Page):
    form_model = 'player'
    form_fields = ['edad', 'genero', 'escolaridad', 'conocimiento_derechos', 'derecho', 'hectareas']

class Risk_aversion(Page):
    form_model='player'
    form_fields=['risk_aversion']

class Elicit_beliefs(Page):
    form_model = 'player'
    form_fields = ['pre_beliefs_mas']

    #@staticmethod
    #def error_message(player, values):
    #   tot = values['pre_beliefs_menos']+values['pre_beliefs_igual']+values['pre_beliefs_mas']
    #   if tot != 10:
    #       return 'Debe repartir las 10 personas entre los 3 grupos'


page_sequence = [Bienvenida, Demographics, Risk_aversion, Elicit_beliefs]
