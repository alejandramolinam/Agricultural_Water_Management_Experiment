import random
import numpy
from otree.api import *

doc = """
One player decides how to divide a certain amount between himself and the other
player.
See: Kahneman, Daniel, Jack L. Knetsch, and Richard H. Thaler. "Fairness
and the assumptions of economics." Journal of business (1986):
S285-S300.
"""

# FUNCTIONS FOR CLASSES
def asignar_turnos(NUM_PARTICIPANTES, NUM_ROUNDS, NUM_TTO):
    NUM_CTRL = NUM_PARTICIPANTES-NUM_TTO

    #Variable Tratamiento
    tratamiento = numpy.empty((NUM_PARTICIPANTES))
    tratamiento[0:NUM_TTO] = 1
    tratamiento[NUM_TTO:] = 0
    print(tratamiento)
    # Asignar parejas entre los TTO
    vecinos = numpy.empty((NUM_PARTICIPANTES, NUM_ROUNDS))
    turnos = numpy.empty((NUM_PARTICIPANTES, NUM_ROUNDS))

    for i in range(0,NUM_ROUNDS):
        indx = list(range(0,NUM_TTO))

        numpy.random.shuffle(indx)
        if (len(indx) % 2) == 1:
            l=len(indx)-1
        else:
            l=len(indx)

        for j in range(0, l, 2):
            vecinos[indx[j],i] = indx[j+1]
            vecinos[indx[j+1],i] = indx[j]
            sorteo_turno=random.randint(0,1)
            turnos[indx[j] , i] = sorteo_turno
            turnos[indx[j + 1], i] = 1 - sorteo_turno

            if (len(indx) % 2) == 1:
                iaux=indx[0:-1]
                vecinos[indx[-1], i] = random.sample(iaux,1)[0]
                turnos[indx[-1], i] = random.randint(0,1)



    for i in range(0,NUM_ROUNDS):
        indx = list(range(NUM_TTO,NUM_PARTICIPANTES))
        numpy.random.shuffle(indx)
        if (len(indx) % 2) == 1:
            l=len(indx)-1
        else:
            l=len(indx)

        for j in range(0, l, 2):
            vecinos[indx[j],i] = indx[j+1]
            vecinos[indx[j+1],i] = indx[j]
            sorteo_turno=random.randint(0,1)
            turnos[indx[j] , i] = sorteo_turno
            turnos[indx[j + 1], i] = 1 - sorteo_turno

            if (len(indx) % 2) == 1:
                iaux=indx[0:-1]
                vecinos[indx[-1], i] = random.sample(iaux,1)[0]
                turnos[indx[-1], i] = random.randint(0,1)

    turnos = turnos + 1
    vecinos = vecinos + 1
    return turnos, vecinos, tratamiento


def asignar_demandas(NUM_PARTICIPANTES, NUM_ROUNDS):
    demandas = numpy.empty((NUM_PARTICIPANTES, NUM_ROUNDS))
    pru_vec = [11, 12]
    pri_vec = [10, 11, 12, 13, 14]
    ver_vec = [12, 13, 14, 15, 16, 16, 17, 18, 19, 20]

    for i in range(NUM_PARTICIPANTES):
        pru = random.sample(pru_vec, k=len(pru_vec))
        pri = random.sample(pri_vec, k=len(pri_vec))
        ver = random.sample(ver_vec, k=len(ver_vec))
        demanda_i = pru + pri + ver
        demanda_i = demanda_i[0:NUM_ROUNDS]
        demandas[i,:] = demanda_i

    return demandas


def asignar_evento1(TURNOS, Ni, Nf):
    evento = list([])
    for p in range(len(TURNOS)):
        t2 = [i for i in range(Ni-1, Nf) if TURNOS[p, i] == 2]
        if len(t2) > 0:
            #evento.append(random.choice(t2)+1)
            evento.append(20)
        else:
            evento.append(0)
    return evento


def asignar_evento2(TURNOS, Ni, Nf):
    evento = list([])
    for p in range(len(TURNOS)):
        t2 = [i for i in range(Ni-1, Nf) if TURNOS[p, i] == 2]
        if len(t2) > 0:
            #evento.append(random.choice(t2)+1)
            evento.append(20)
        else:
            evento.append(0)
    return evento


def asignar_fiscalizacion(NUM_PARTICIPANTES, NUM_ROUNDS, PROB_MULTA):
    rand_fiscalizacion = numpy.random.randint(0, 100, (NUM_PARTICIPANTES, NUM_ROUNDS))
    rand_fiscalizacion[:, 0] = 100
    rand_fiscalizacion[:, 1] = 100
    fiscalizacion =  rand_fiscalizacion < PROB_MULTA
    return fiscalizacion


class C(BaseConstants):
    NAME_IN_URL = 'agua_tto'
    PLAYERS_PER_GROUP = 10
    NUM_ROUNDS = 17
    RONDA_FIN_PRUEBA = 2
    RONDA_INICIO_ESCASEZ = 8
    NUM_PARTICIPANTES = PLAYERS_PER_GROUP
    NUM_TTO = 10
    ENDOWMENT = 24 # Initial amount allocated to the dictator
    VALOR_TOKEN = 100 # Pesos chilenos
    PROB_MULTA = 20 # porcentaje
    GANANCIA_INICIAL = 10000
    [t, v, tto] = asignar_turnos(NUM_PARTICIPANTES, NUM_ROUNDS, NUM_TTO)
    TURNOS = t
    VECINOS = v
    TRATAMIENTO = tto
    EVENTO1 = asignar_evento1(TURNOS, RONDA_FIN_PRUEBA+1, RONDA_INICIO_ESCASEZ-1)
    EVENTO2 = asignar_evento2(TURNOS, RONDA_INICIO_ESCASEZ, NUM_ROUNDS)
    DEMANDAS = asignar_demandas(NUM_PARTICIPANTES, NUM_ROUNDS)
    FISCALIZACION = asignar_fiscalizacion(NUM_PARTICIPANTES,NUM_ROUNDS,PROB_MULTA)


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    demand = models.IntegerField(initial=12)
    kept = models.IntegerField(
        doc="""Amount dictator decided to keep for himself""",
        min=0,
        max=C.ENDOWMENT,
        label="",
        initial=12
    )
    left = models.IntegerField(initial=12)
    turno = models.IntegerField(initial=0)
    deficit = models.IntegerField(initial=0)
    perdida = models.IntegerField(initial=0)
    vecino = models.IntegerField(initial=0)
    kept_vecino = models.IntegerField(initial=12)
    dem_vecino = models.IntegerField(initial=12)
    finalpayment = models.CurrencyField(initial=C.GANANCIA_INICIAL)
    evento_externo1 = models.IntegerField(initial=0)
    evento_externo2 = models.IntegerField(initial=0)
    fiscalizacion = models.BooleanField()
    multa = models.CurrencyField(initial=0)
    tratamiento = models.IntegerField(initial=0)

# FUNCTIONS
def creating_session(subsession: Subsession):
    for player in subsession.get_players():
        id = player.id_in_group
        player.tratamiento = int(C.TRATAMIENTO[id - 1])
        #print(player.tratamiento)


def update_demand(group: Group):
    ronda = group.round_number
    players = group.get_players()
    for p in players:
        id = p.id_in_group
        p.demand = int(C.DEMANDAS[id-1,ronda-1])
        p.vecino = int(C.VECINOS[id-1,ronda-1])

    dems = [p.demand for p in players]
    for p in players:
        p.dem_vecino = dems[p.vecino-1]


def set_payoffs(group: Group):
    players = group.get_players()
    ronda = group.round_number
    kepts = [p.kept for p in players]

    for p in players:
        id = p.id_in_group
        p.turno = int(C.TURNOS[id-1,ronda-1])  # 1:J1 2:J2
        p.fiscalizacion = int(C.FISCALIZACION[id-1,ronda-1])
        p.evento_externo1 = C.EVENTO1[id - 1]
        p.evento_externo2 = C.EVENTO2[id - 1]
        if ronda == C.EVENTO1[id-1]:
            p.kept_vecino = 12
            left_vecino = random.randint(5, 9)
            p.deficit = int(max(0, p.demand - left_vecino))
            p.perdida = p.deficit * C.VALOR_TOKEN  # pago como J2
            #p.payoff = p.pago_final-  # pago como J2
        elif ronda == C.EVENTO2[id-1]:
            p.kept_vecino = 12
            left_vecino = random.randint(5, 11)
            #p.payoff = min(left_vecino, p.demand) * C.VALOR_TOKEN  # pago como J2
            p.deficit = int(max(0, p.demand - left_vecino))
            p.perdida = p.deficit * C.VALOR_TOKEN  # pago como J2
        else:
            if p.turno == 1:
                p.deficit = int(max(0, p.demand - p.kept))
                p.perdida = p.deficit * C.VALOR_TOKEN
            else:
                #if (ronda > C.RONDA_FIN_PRUEBA) and (ronda < C.RONDA_INICIO_ESCASEZ):
                if ronda < (C.RONDA_FIN_PRUEBA + 1):
                    p.kept_vecino = 12
                else:
                    p.kept_vecino = kepts[p.vecino-1]

                left_vecino = C.ENDOWMENT - p.kept_vecino
                #p.payoff = min(left_vecino, p.demand) * C.VALOR_TOKEN  # pago como J2
                p.deficit = int(max(0, p.demand - left_vecino))
                p.perdida = p.deficit * C.VALOR_TOKEN

        if p.turno == 1 and p.fiscalizacion:
            p.multa = max(0, p.kept - 12)*C.VALOR_TOKEN*0.5
        else:
            p.multa = 0

        if ronda == 1:
            p.finalpayment = C.GANANCIA_INICIAL - p.perdida - p.multa
        elif ronda == (C.RONDA_FIN_PRUEBA+1):
            p.finalpayment = C.GANANCIA_INICIAL - p.perdida - p.multa
        else:
            p_anterior = p.in_round(ronda-1)
            p.finalpayment = p_anterior.finalpayment - p.perdida - p.multa


# PAGES
class Inicializar_juego(Page):
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1


class Introduction_ctrl(Page):
    @staticmethod
    def is_displayed(player):
        display = (player.round_number == 1) & (player.tratamiento == 0)
        return display


class Introduction_tto(Page):
    @staticmethod
    def is_displayed(player):
        display = (player.round_number == 1) & (player.tratamiento == 1)
        return display


class Demand(WaitPage):
    after_all_players_arrive = update_demand
    title_text = "Esperando al otro jugador"
    body_text = ""
    @staticmethod
    def is_displayed(player):
        return True


class Offer_tto(Page):
    form_model = 'player'
    form_fields = ['kept']

    @staticmethod
    def vars_for_template(player: Player):
        ronda = player.round_number
        if ronda==1:
            fpay=C.GANANCIA_INICIAL
        elif ronda==C.RONDA_FIN_PRUEBA+1:
            fpay = C.GANANCIA_INICIAL
        else:
            p_anterior = player.in_round(ronda - 1)
            fpay = round(p_anterior.finalpayment)


        demandrect = round(400 * player.demand / 24)
        demandrect_vecino = round(400 * player.dem_vecino / 24)
        offsetrect_vecino = round(400 - demandrect_vecino) + 10
        gananciarect = round(300 * fpay / C.GANANCIA_INICIAL)
        ygananciarect = 10 + 300 - gananciarect
        ygananciatext = 15 + 300 - gananciarect
        ronda = int(player.round_number)
        return dict(demandrect=demandrect, ronda=ronda, rondatitle=ronda - 2,
                    gananciarect=gananciarect, fpay=fpay,
                    ygananciarect=ygananciarect, ygananciatext=ygananciatext,
                    demandrect_vecino=demandrect_vecino, offsetrect_vecino=offsetrect_vecino)

    @staticmethod
    def is_displayed(player):
        return player.tratamiento == 1


class Offer_ctrl(Page):
    form_model = 'player'
    form_fields = ['kept']

    @staticmethod
    def vars_for_template(player: Player):
        ronda = player.round_number
        if ronda==1:
            fpay=C.GANANCIA_INICIAL
        elif ronda==C.RONDA_FIN_PRUEBA+1:
            fpay = C.GANANCIA_INICIAL
        else:
            p_anterior = player.in_round(ronda - 1)
            fpay = round(p_anterior.finalpayment)


        demandrect = round(400 * player.demand / 24)
        demandrect_vecino = round(400 * player.dem_vecino / 24)
        offsetrect_vecino = round(400 - demandrect_vecino) + 10
        gananciarect = round(300 * fpay / C.GANANCIA_INICIAL)
        ygananciarect = 10 + 300 - gananciarect
        ygananciatext = 15 + 300 - gananciarect
        ronda = int(player.round_number)
        return dict(demandrect=demandrect, ronda=ronda, rondatitle=ronda - 2,
                    gananciarect=gananciarect, fpay=fpay,
                    ygananciarect=ygananciarect, ygananciatext=ygananciatext,
                    demandrect_vecino=demandrect_vecino, offsetrect_vecino=offsetrect_vecino)

    @staticmethod
    def is_displayed(player):
        return player.tratamiento == 0

class Pagos(WaitPage):
    title_text = "Sorteando turnos"
    body_text = "Cuando todos hayan terminado de decidir se le informará que turno le tocó"
    after_all_players_arrive = set_payoffs

class Results(Page):
    @staticmethod
    def vars_for_template(player: Player):
        group = player.group
        ronda = int(group.round_number)

        if player.turno == 1:
            left = 24 - player.kept
            # ancho rectangulos
            hriegorect = round(400 * player.kept / 24)
            demandrect = round(400 * player.demand / 24)
            offshriego = 0  # no se usa en player 1
            offsetrect = 0  # no se usa en player 1
            # posiciones textos en eje x
            texthriego = round(hriegorect / 2) + 5
            texthrvecino = round((400 - hriegorect) / 2) + 5 + hriegorect
            textdeficit = round(400 * (player.kept + round(player.deficit/2)) / 24) + 5
            # linea de decision riego (fin del rectangulo hriegorect)
            linehriego = hriegorect + 10

        else:
            left=C.ENDOWMENT-player.kept_vecino
            # ancho rectangulos
            hriegorect = round(400 * left / 24)
            offshriego = round(400 - hriegorect) + 10
            demandrect = round(400 * player.demand / 24)
            offsetrect = round(400 - demandrect) + 10
            # posiciones textos en eje x
            texthriego = round(400 - hriegorect / 2) + 5
            texthrvecino = round(400 * player.kept_vecino / 24 / 2) + 5
            textdeficit = 400 - round(400 * (left + player.deficit / 2) / 24) + 5
            # linea de decision riego (fin del rectangulo hriegorect)
            linehriego = offshriego


        fpay = round(player.finalpayment)
        gananciarect = round(300 * fpay / C.GANANCIA_INICIAL)
        ygananciarect = 10 + 300 - gananciarect
        ygananciatext = 15 + 300 - gananciarect

        return dict(left=left, ronda=ronda, rondatitle=ronda - 2,
                    demandrect=demandrect, offsetrect=offsetrect,
                    hriegorect=hriegorect, offshriego=offshriego,
                    textdeficit=textdeficit, texthriego=texthriego,
                    linehriego=linehriego, texthrvecino=texthrvecino,
                    gananciarect=gananciarect, fpay=fpay,
                    ygananciarect=ygananciarect, ygananciatext=ygananciatext)


class PagoFinal(Page):
    @staticmethod
    def is_displayed(player):
        return player.round_number == C.NUM_ROUNDS
    #@staticmethod
    #def vars_for_template(player: Player):
        #Nrondapago = random.randint(4, C.NUM_ROUNDS-1)
        #player_in_selected_round = player.in_round(Nrondapago)
        #player.finalpayment = player_in_selected_round.payoff
        #return dict(pagofinal=player.finalpayment, Nrondapago=Nrondapago-3)


page_sequence = [Inicializar_juego, Demand, Offer_ctrl, Offer_tto, Pagos, Results, PagoFinal]
