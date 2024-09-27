from os import environ

SESSION_CONFIGS = [
    dict(
        name='experimento_agua_control',
        app_sequence=['survey_inicial','agricultores_postpilotaje_ctrl','survey_final'],
        num_demo_participants=2,
        players_per_group=2
	),
    dict(
        name='experimento_agua_tratamiento',
        app_sequence=['survey_inicial','agricultores_postpilotaje_tto','survey_final'],
        num_demo_participants=2,
        players_per_group=2
	)
]

# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    real_world_currency_per_point=500, participation_fee=5000, doc=""
)

PARTICIPANT_FIELDS = ['Payoff']
SESSION_FIELDS = ['turnos','vecinos','tratamientos','demandas','fiscalizacion']

# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'es'

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'pesos'
USE_POINTS = False

ROOMS = [
    dict(
        name='agua_s1',
        display_name='Experimento Agua S1',
        participant_label_file='_rooms/agua.txt',
    ),
    dict(
        name='agua_s2',
        display_name='Experimento Agua S2',
        participant_label_file='_rooms/agua.txt',
    ),
    dict(
        name='agua_s3',
        display_name='Experimento Agua S3',
        participant_label_file='_rooms/agua.txt',
    ),
    dict(
        name='agua_s4',
        display_name='Experimento Agua S4',
        participant_label_file='_rooms/agua.txt',
    )
]

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')


DEMO_PAGE_INTRO_HTML = """
Here are some oTree games.
"""

SECRET_KEY = '1138103902687'

INSTALLED_APPS = ['otree']
