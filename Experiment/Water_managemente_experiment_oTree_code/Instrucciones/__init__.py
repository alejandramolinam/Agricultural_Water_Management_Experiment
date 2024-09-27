from otree.api import *


class C(BaseConstants):
    NAME_IN_URL = 'instrucciones'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    pass

# FUNCTIONS
# PAGES
class Instrucciones_intro(Page):
    pass

class Instrucciones_wait(WaitPage):
    pass

class Instrucciones_video_parte1(Page):
    pass

class Instrucciones_video_parte2(Page):
    pass


page_sequence = [Instrucciones_intro, Instrucciones_wait,
                 Instrucciones_video_parte1, Instrucciones_wait, Instrucciones_video_parte2]
