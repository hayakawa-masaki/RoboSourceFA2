1 ' ===================================
2 '
3 '  21053001 STEP5 Assy5�v���O����
4 '
5 ' �쐬�ҁF������T
6 ' �쐬���F2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1���痬�p
8 ' ===================================
9 '===== <Insight�萔> =====
10 '===== <Insight�ϐ���`> =====
11 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
12 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
13 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
14 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
15 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
16 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
17 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
18 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
19 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
20 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
21 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
22 '
23 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
26 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
27 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
28 '
29 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
30 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
33 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
34 '��Ɨp�ϐ�
35 Def Inte MInspErrNum                '�������s�G���[�ԍ�
36 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
37 Def Inte MRtn                       'Function�߂�l�擾�p
38 Def Inte MRtn2                      'Function�߂�l�擾�p
39 Def Inte MRet3                      'Function�߂�l�擾�p
40 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
41 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
42 Def Inte MovrdA                     '�l�W����Ovrd �ϗp
43 Def Float MSpdA                     '�l�W����Spd�@�ϗp
44 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
45 '===== <Insight�ϐ��ݒ�> =====
46 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
47 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
48 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
49 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
50 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
51 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
52 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
53 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
54 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
55 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
58 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
59 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
60 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
61 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
62 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
63 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
64 '===== <�d�h���ϐ���`> =====
65 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
66 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
67 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
68 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
69 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
70 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
71 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
72 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
73 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
74 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
75 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
76 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
77 Y60_Driver=12240 '�d�h�������v��� CCW
78 Y61_Driver=12241 '�d�h�����v��� CW
79 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
80 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
81 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
82 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
83 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
84 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
85 X34_ScrewReady1=11259 '�˂�����1�@Read
86 '===== <�d�h���萔> =====
87 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
88 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <���{�b�g�萔> =====
92 '===== <���{�b�g�ϐ���`> =====
93 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 '�E�B���h��ʔԍ��ݒ�
102 MWindReSet = 0
103 MWindInfoScr = 5
104 MWindErrScr = 10
105 MWindErrScr2 = 11
106 MWindErrScr3 = 13
107 MWindErrScr17 = 17
108 MWindErrScr18 = 18
109 MWindCmmnScr = 20
110 MWindJigRelase19049 = 60
111 MWindJigRelase19050 = 61
112 MWindJigRelase19051 = 62
113 '
114 MClear% = 0        'KEY_�̃N���A
115 MAbout% = 1        'KEY_��~
116 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
117 MContinue% = 3     'KEY_�p�� �ēx����������s��
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '�g������
123 MPass% = 7         '�H���p�X
124 MPiasNG% = 8       'Pias�m�F������NG
125 '
126 '�������pKEY�ԍ�   '
127 MRobotInit1% = 11  '�����ʒu�p
128 MRobotInit2% = 12  '�����ʒu�p
129 MRobotInit3% = 13  '�����ʒu�p
130 MRobotInit4% = 14  '�����ʒu�p
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
133 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
134 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
135 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
138 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
139 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
140 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
141 '
142 MOK% = 1               '�e����p
143 MNG% = 0               '�e����p
144 MTIMEOUT% = -1         '�e����p
145 MJudge% = 0            '������i�[�p
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
149 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
150 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
151 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
152 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
153 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
154 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
155 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
156 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
157 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
158 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
161 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
162 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
163 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
164 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
165 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
169 MIN_PIAS_MyProcessComp% = 11573        '���H����������
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
171 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
174 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
175 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
176 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
177 '
178 MOUT_PiasAssyResultOK% = 12549    '�g��OK
179 MOUT_PiasAssyResultNG% = 12550    '�g��NG
180 MOUT_PiasAssyResultWr% = 12548    '�H��������������
181 '
182 MIN_PiasProcessNG% = 11559        '�H����������NG
183 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
184 MIN_PiasProcessOK% = 11558        '�H����������OK
185 '
186 MIN_Insight_Use% = 11369               '�摜�m�FON
187 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
191 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
196 '
197 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
198 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
201 '
202 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
203 '
204 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '������ ���Z�ϐ�
211 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
212 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
213 Def Inte MSuctionErrQty     '�z���G���[�� 2022/04/27 �n��
214 Def Inte nAssyOkQty         '���g�p
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO�ϐ���`> =====
218 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
219 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
220 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
221 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
222 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
223 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
224 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
225 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
226 '
227 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
228 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
229 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
230 '
231 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
232 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
233 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
234 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
235 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
236 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
237 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
238 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
239 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
240 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
241 '
242 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
243 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
244 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
245 '
246 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
247 '
248 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
249 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
250 '
251 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
252 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
253 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
254 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
255 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
256 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
261 '
262 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
263 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
264 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
265 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
266 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
267 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
268 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
269 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
270 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
271 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
272 '
273 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
274 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
275 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
276 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
277 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
278 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
279 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
280 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
281 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
282 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
283 '
284 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
285 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
286 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
287 '
288 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
293 '
294 '����
295 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
296 Def Inte MOn                            '�o��=1
297 Def Inte MOff                           '�o��=0
298 '
299 '�˂����ߑ��u_�o�̓A�h���X
300 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
301 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
302 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
303 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
304 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
305 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
307 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
308 '�˂����ߑ��u_���̓A�h���X
309 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
310 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
311 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
312 Def Inte MIN_ScwT_Case1                 '����1��~����M
313 Def Inte MIN_ScwT_Case2                 '����2��~����M
314 Def Inte MIN_ScwT_Case3                 '����3��~����M
315 Def Inte MIN_ScwT_Case4                 '����4��~����M
316 Def Inte MIN_ScwT_Case5                 '����5��~����M
317 '
318 Dim MScwT_Case1%(2)               '����1��~�ϐ�
319 Dim MScwT_Case2%(2)               '����2��~�ϐ�
320 Dim MScwT_Case3%(2)               '����3��~�ϐ�
321 Dim MScwT_Case4%(2)               '����4��~�ϐ�
322 Dim MScwT_Case5%(2)               '����5��~�ϐ�
323 '
324 '����
325 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
326 MOn% = 1                                 '�o�� = 1
327 MOff% = 0                                '�o�� = 0
328 '
329 '�˂����ߋ@_�A�h���X�ݒ�
330 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
331 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
332 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
333 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
334 MOUT_ScwT_Case1OK% = 12874              '����1��~��M�𑗐M
335 MOUT_ScwT_Case2OK% = 12875              '����2��~��M�𑗐M
336 MOUT_ScwT_Case3OK% = 12876              '����3��~��M�𑗐M
337 MOUT_ScwT_Case4OK% = 12877              '����4��~��M�𑗐M
338 MOUT_ScwT_Case5OK% = 12878              '����5��~��M�𑗐M
339 '
340 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
341 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
342 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
343 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
344 MIN_ScwT_Case1% = 11882                 '����1��~�ҋ@����M
345 MIN_ScwT_Case2% = 11883                 '����2��~�ҋ@����M
346 MIN_ScwT_Case3% = 11884                 '����3��~�ҋ@����M
347 MIN_ScwT_Case4% = 11885                 '����4��~�ҋ@����M
348 MIN_ScwT_Case5% = 11886                 '����5��~�ҋ@����M
349 '
350 MScwT_Case1%(1) = MIN_ScwT_Case1%
351 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
352 MScwT_Case2%(1) = MIN_ScwT_Case2%
353 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
354 MScwT_Case3%(1) = MIN_ScwT_Case3%
355 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
356 MScwT_Case4%(1) = MIN_ScwT_Case4%
357 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
358 MScwT_Case5%(1) = MIN_ScwT_Case5%
359 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
360 '
361 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
362 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
363 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
364 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
365 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
366 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
367 Def Inte MRecoveryPass      '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
368 Def Inte MJ6          'J6���̒l���r����ׂ̕ϐ�
369 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
370 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
371 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
372 '
373 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
374 Function M% fnAssyStart
375     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
376 '   BaseUnit6�ʐM�m�F
377     *RE_COM_CHECK
378     MRtn = 1    '������
379     If M_In(11920) = 0 Then     'BaseUnit6���E�����̃t���O�𗧂ĂĂ��Ȃ�
380         If M_In(11930) = 0 And M_In(11931) = 0 Then   '�ʐM�ɂĉ�]�p�s��
381             Dly 2.3                                   '��]�҂�
382             If M_In(11930) = 0 And M_In(11931) = 0 Then  '������x�m�F
383                 MRtn = 0                              '�ʐM�ɂĈُ�
384                 Break
385             EndIf
386             Break
387         EndIf
388         Break
389     EndIf
390     If MRtn = 1 Then GoTo *BU6Com_OK    '�ʐMOK�Ȃ烉�x���W�����v
391     fErrorProcess(11,298,287,0)         '0,284��298,287�ɕύX6/3����
392     If M_20# = MNext% Then M_20# = MClear%
393     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
394     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
395     If M_20# = MContinue% Then GoTo *RE_COM_CHECK
396     *BU6Com_OK
397 '
398 '
399 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
400     M_20# = MClear%                       '������
401 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
402 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
403 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
404 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
405 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
406 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
407 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
408 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
409 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
410 '    EndIf
411 '    '
412 '    '���W�ړ�
413 '    '
414 '    '����xx��~
415 '    fScewTCaseStop(MScwT_Case5%)
416 '    '
417 '    '�x�[�X���j�b�gKEY
418 '    Wait M_In(MTEST_KEY%) = MOn%
419 '    '
420 '    '�ĊJ�n
421 '    fScewTReStart()
422 '    '
423 '    '���W�ړ�
424 '    '
425 '    '�˂����ߊ���
426 '    Mret% = fScewTFinish()
427 ' �l�W���߃e�X�g�I��
428 ' PIAS�e�X�g -----------
429 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
430 '    MRet% = fnPiasWrite(MNG%)
431  '   MRet% = fnPCBNumberCheck()
432 ' PIAS�e�X�g�I�� -------
433 '
434     '�g���J�n(9/6�ǉ�(����))
435     '�v���O�������_
436     Ovrd 100
437     ' �n���h��ԏ�����(10/29�ǉ�M.H)(2/11�C��(����))
438     Cmp Off                     '�R���v���C�A���X���[�h�I��
439     ColChk On                   '�Փˌ��mON
440     If M_In(11266) Then
441         M_Out(12256) = 0
442         M_Out(12257) = 1
443     EndIf
444     If M_In(11269) Then
445         M_Out(12258) = 0
446         M_Out(12259) = 1
447     EndIf
448     If M_In(11271) Then
449         M_Out(12260) = 0
450         M_Out(12261) = 1
451     EndIf
452     *WAIT_HAND_INI
453     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
454     *CompHandIni
455     M_Out(12257) = 0
456     M_Out(12259) = 0
457     M_Out(12261) = 0
458 '
459 '
460 'Dly 10                                  '�f�o�b�O�p(22/09/30����)
461     ' �˂����ߋ@�e�X�g�p ----------
462      Mret% = fScrewTcomChk()
463     If Mret% = -1 Then GoTo *ASSY_ERROR_END
464     ' �˂����ߋ@�ʐM�J�n
465 '    fScrewTStart()           '�����ʒu�ύX2/27����
466     '�`�P�b�gID��ǂ�
467     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
468     PTemp = P_Curr
469     MRtn = 0
470 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
471 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
472 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
473 '                MRtn = 1
474 '            EndIf
475 '        EndIf
476 '    EndIf
477 '    If MRtn = 1 Then
478 '        Mov PTicketRead
479 '    Else
480 '        Cnt 1 , 10 , 10
481 '        Mov PInitialPosition
482 '        Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
483 '        Cnt 0
484 '        Mvs PTicketRead             'ID�ǂ݈ʒu
485 '    EndIf
486 '
487 ' 2022/04/12 ���S�����֏����ύX �n��
488 ' PInitialPosition �ݐ� MStandby=2
489 ' PTicketRead_1 �ݐ� MStandby=1
490 '
491     MStandby = 0    '�ҋ@�ʒu�t���O��������
492     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
493         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
494             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
495                 MStandby = 2
496             EndIf
497         EndIf
498     EndIf
499     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
500         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
501             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
502                 MStandby = 1
503             EndIf
504         EndIf
505     EndIf
506     If MStandby = 2 Then
507         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
508         Cnt 0
509     EndIf
510     If MStandby <> 0 Then GoTo *PositionOK
511     fErrorProcess(11,230,281,0)            '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
512     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
513     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
514     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
515     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
516     *PositionOK
517 '
518     Mvs PTicketRead             'ID�ǂ݈ʒu
519 '
520     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
521     M_Out(12258) = 1            'DVD���J�`���b�N��ON
522 '
523     '
524     MRtn = 1        'MRtn������
525 *RE_TICKET_READ
526 '    MRtn = fnPiasCheck()               'ID�ǂݎ��
527 'PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
528 'MInspGroup%(1) = 1              '����G�ԍ�
529 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
530 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
531     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
532     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
533     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
534 EndIf
535 If MRtn = 1 Then GoTo *CompRead
536 '
537     '�G���[�����i�ʒu���߂�����
538 *RE_ERR_REL_1
539 If M_20# = MContinue% Then M_20# = MRtn
540 M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
541 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
542 '
543 If MRtn = 1 Then GoTo *CompErrorRelease
544 MRtn = M_20#        'M_20#�ꎞ���
545 M_20# = MClear%
546 fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
547 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
548 If M_20# = MNext% Then M_20# = MRtn
549 If M_20# = MNgProcess% Then M_20# = MAbout%
550 *CompErrorRelease
551 '
552 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
553 If M_20# = MNext% Then M_20# = MPass%
554 Mvs PTicketRead_1                         '22/04/07 �ǉ� �n��
555 GoTo *ASSY_ERROR_END
556 *CompRead
557     fScrewTStart()           '�����ʒu�ύX2/27����)
558 '
559 '
560 ''    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu�ύX2/7����)
561 '    MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
562 '    *RE_ERR_REL_2
563 '    If M_20# = MContinue% Then M_20# = MRtn2
564 '    If MRtn = 0 Then
565 '        MRtn2 = 1       'MRtn2������
566 '        M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
567 '        Mov PInitialPosition  '"�C�j�V�����ɖ߂铮��"
568 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
569 '        If MRtn2 = 0 Then
570 '            MRtn2 = M_20#                   '�ʒu���ߖߒ[�G���[�Ȃ�M_20#�������ꎞ���
571 '            M_20# = MClear%                 'M_20#������
572 '            fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
573 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#�ɔ����l����
574 '                '�ʒu���߃G���[���������čH���𔲂���ꍇ��~�������s��
575 '            If M_20# = MNgProcess% Then M_20# = MAbout%
576 '            Break
577 '        EndIf
578 '        Break
579 '            EndIf
580 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
581 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
582     '
583     '�p���b�g���琻�i�����
584     '
585     *RE_POSITIONING
586     '
587     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
588 '    Wait M_In(11273) = 1     '�{�̈ʒu���ߏo�[���o
589     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '�{�̈ʒu���ߏo�[���o
590     If MRtn = 1 Then GoTo *CompPositioning
591     fErrorProcess(11,231,282,0)
592     If M_20# = MNext% Then M_20# = MClear%
593     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
594     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
595     If M_20# = MContinue% Then GoTo *RE_POSITIONING
596     *CompPositioning
597 '
598     Mov PProductOnPltGet_2      '�{�̎󂯎������_
599 '
600 ''    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu�ύX3/14����)
601 '    MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
602 '    *RE_ERR_REL_2
603 '    If M_20# = MContinue% Then M_20# = MRtn2
604 '    If MRtn = 0 Then
605 '        MRtn2 = 1       'MRtn2������
606 '        M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
607 '        Mov PInitialPosition  '"�C�j�V�����ɖ߂铮��"
608 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
609 '        If MRtn2 = 0 Then
610 '            MRtn2 = M_20#                   '�ʒu���ߖߒ[�G���[�Ȃ�M_20#�������ꎞ���
611 '            M_20# = MClear%                 'M_20#������
612 '            fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
613 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#�ɔ����l����
614 '                '�ʒu���߃G���[���������čH���𔲂���ꍇ��~�������s��
615 '            If M_20# = MNgProcess% Then M_20# = MAbout%
616 '            Break
617 '        EndIf
618 '        Break
619 '            EndIf
620 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
621 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
622 '
623 '    Mov PProductOnPltGet_1      '�{�̎󂯎����
624     '
625     *RE_PLT_GET_1
626     '
627     M_Out(12256) = 0            '�{�̃`���b�N��OFF
628     M_Out(12257) = 1            '�{�̃`���b�N�JON
629     '
630 '    Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
631     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
632     If MRtn = 1 Then GoTo *CompPltGet1
633     fErrorProcess(11,244,284,0)
634     If M_20# = MNext% Then M_20# = MClear%
635     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
636     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
637     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
638     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
639     *CompPltGet1
640     '
641     Mov PProductOnPltGet_1      '�{�̎󂯎����
642     '
643     Ovrd 25
644 '    Fine 0.05 , P
645     Mvs PProductOnPltGet        '�{�̎󂯎��ʒu
646     Dly 0.1
647     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
648     M_Out(12256) = 1            '�{�̃`���b�N��ON
649 '    Fine 0 , P
650     '
651     M_Out(12263) = 1 Dly 0.5                    '�{�̈ʒu���ߖߒ[ON
652 '    Wait M_In(11274) = 1     '�{�̈ʒu���ߖߒ[���o
653     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '�{�̈ʒu���ߖߒ[���o
654     If MRtn = 1 Then GoTo *CompPltGet2
655     M_Out(12256) = 0                            '�{�̃`���b�N��OFF
656     M_Out(12257) = 1                            '�{�̃`���b�N�JON
657     Dly 2.0
658     Mvs PProductOnPltGet_1
659     Mov PProductOnPltGet_2
660     fErrorProcess(11,234,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
663     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
664     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
665     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
666     Mov PProductOnPltGet_1
667     Mvs PProductOnPltGet
668     M_Out(12257) = 0                            '�{�̃`���b�N�JOFF
669     M_Out(12256) = 1                            '�{�̃`���b�N��ON
670     Dly 2.0
671     *CompPltGet2
672     '
673 '    Wait M_In(11264) = 1        '�{�̌��o
674     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '�{�̌��o
675     If MRtn = 1 Then GoTo *CompPltGet3
676     M_Out(12256) = 0            '�{�̃`���b�N��OFF
677     M_Out(12257) = 1            '�{�̃`���b�N�JON
678     Dly 2.0
679     Mvs PProductOnPltGet_1
680     Mov PProductOnPltGet_2
681     fErrorProcess(11,252,284,0)
682     If M_20# = MNext% Then M_20# = MClear%
683     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
684     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
685     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
686     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
687     Mov PProductOnPltGet_1
688     Mvs PProductOnPltGet
689     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
690     M_Out(12256) = 1            '�{�̃`���b�N��ON
691     Dly 2.0
692     *CompPltGet3
693     '
694 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
695     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
696     If MRtn = 1 Then GoTo *CompPltGet4
697     M_Out(12256) = 0            '�{�̃`���b�N��OFF
698     M_Out(12257) = 1            '�{�̃`���b�N�JON
699     Dly 2.0
700     Mvs PProductOnPltGet_1
701     Mov PProductOnPltGet_2
702     Dly 0.1
703     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
704     M_Out(12256) = 1            '�{�̃`���b�N��ON
705     Dly 3.0
706     fErrorProcess(11,245,284,0)
707     If M_20# = MNext% Then M_20# = MClear%
708     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
709     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
710     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
711     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
712     M_Out(12256) = 0            '�{�̃`���b�N��OFF
713     M_Out(12257) = 1            '�{�̃`���b�N�JON
714     Dly 2.0
715     Mov PProductOnPltGet_1
716     Mvs PProductOnPltGet
717     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
718     M_Out(12256) = 1            '�{�̃`���b�N��ON
719     Dly 2.0
720     *CompPltGet4
721     '
722     Dly 0.1                     '�O�̂��߃f�B���C(0.2��0.1�ɕύX221218����)
723     Cnt 1 , 100 , 100           '�͈͕ύX(10��100�A221219����)
724     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
725     Mvs PProductOnPltGet_1      '�{�̎󂯎����
726     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
727     Ovrd 100
728     Accel 50 , 50
729     Mov PProductOnPltGet_2      '�{�̎󂯎������_
730     '
731     '���i���˂����{2�ɒu��
732     Mov PProductOnRoboSet_3     '�o�H
733     Accel 100 , 100
734     Cnt 0
735 '    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu,�����ύX3/1����)
736     MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
737     *RE_ERR_REL_2
738     If MRtn = 0 Then
739         Cnt 0
740         Mov PProductOnPltSet_2
741         Mov PProductOnPltSet_1
742         Mvs PProductOnPltSet
743         M_Out(12256) = 0        '�{�̃`���b�N��OFF
744         M_Out(12257) = 1        '�{�̃`���b�N�JON
745         Dly 2.0
746         Mvs PProductOnPltSet_1
747         Mvs PProductOnPltSet_2
748         Mov PInitialPosition
749     EndIf
750     If MRtn = 0 Then GoTo *ASSY_ERROR_END
751     '
752     *RE_ROBO_SET_1
753     '
754     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
755     M_Out(12258) = 1            'DVD���J�`���b�N��ON
756 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
757     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
758     If MRtn = 1 Then GoTo *CompRoboSet1
759     fErrorProcess(11,269,284,0)
760     If M_20# = MNext% Then M_20# = MClear%
761     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
762     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
763     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
764     *CompRoboSet1
765 '
766     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
767 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
768     Ovrd 25
769     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
770     Mvs PProductOnRoboSet       '�˂����{���i�u���ʒu
771     M_Out(12866) = 1 Dly 0.3    '�˂����{2����ĊJ(��~1�`��~2)
772 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
773     MScrewRoboNgFlg% = 0
774     MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
775     If MRtn = 0 Then
776         MScrewRoboNgFlg% = 1
777     EndIf
778 '
779     *RE_ROBO_SET_2
780 '
781     M_Out(12256) = 0            '�{�̃`���b�N��OFF
782     M_Out(12257) = 1            '�{�̃`���b�N�JON
783 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
784     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
785     If MRtn = 1 Then GoTo *CompRoboSet2
786     fErrorProcess(11,244,284,0)
787     If M_20# = MNext% Then M_20# = MClear%
788     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
789     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
790     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
791     *CompRoboSet2
792     '
793     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
794     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
795 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
796     Ovrd 100
797     Cnt 1 , 10 , 10
798     Mov PProductOnRoboSet_3     '�o�H
799     '
800     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
801     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
802 '
803 '
804 '
805     '
806     '�`���g�X���C�_�[������
807     Cnt 1 , 10
808     Mov PPushTilt_3             '�`���g�X���C�_�[���������ς����_
809     Cnt 0
810     Mov PPushTilt_2             '�`���g�X���C�_�[�������_
811     Ovrd 30
812     Mvs PPushTilt_1             '�`���g�X���C�_�[�������
813     Spd 1000
814     Ovrd 5
815     Mvs PPushTilt               '�`���g�X���C�_�[����
816     Spd M_NSpd
817     Ovrd 50
818     Cnt 1 , 1 , 1
819     Mvs PPushTilt_1             '�`���g�X���C�_�[�������
820     Cnt 1 , 10 , 10
821     Ovrd 100
822     Mov PPushTilt_2             '�`���g�X���C�_�[�������_
823     Cnt 1 , 100 , 100           'Cnt��100mm�ߖT�Œǉ�(221219����)
824     '
825     '�w�ʔ����(�R���v���C�A���X���[�h����11/8����)
826     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
827 '    Cnt 1 , 10'�b��
828     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~2�`��~3)
829 '    MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
830 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
831 '    Mov PPlateBackGet_1         '�w�ʔ󂯎����'�b��
832     Cnt 0'�b��
833     Mov PPlateBackGet_1         '�w�ʔ󂯎����
834     '
835     *RE_PLATE_GET
836     '
837     Fine 0.05 , P               '�t�@�C������ON
838     Ovrd 25
839     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
840 '    Dly 0.2                     '�ꎞ�R�����g�A�E�g
841     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
842     M_Out(12256) = 1            '�{�̃`���b�N��ON
843     '
844 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
845     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
846     If MRtn = 1 Then GoTo *CompPlateGet_1
847     M_Out(12256) = 0            '�{�̃`���b�N��OFF
848     M_Out(12257) = 1            '�{�̃`���b�N�JON
849     Mvs PPlateBackGet_1
850     fErrorProcess(11,245,293,0)     '284��293�ɕύX6/3����
851     If M_20# = MNext% Then M_20# = MClear%
852     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
853     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
854     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
855     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
856     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
857     M_Out(12256) = 1            '�{�̃`���b�N��ON
858     *CompPlateGet_1
859     Fine 0 , P                  '�t�@�C������OFF
860     '
861     Ovrd 5
862     Accel 25 , 100
863     Dly 0.7                     '�f�B���C���Ԓ��ߒ�(�c���͊m��)
864 '    CmpG 0.7,0.7,,,,,,       'X,Y���Q�C����0.7�ɕύX
865 '    ColChk Off                  '�Փˌ��mOFF
866 '    Cmp Pos , &B11          'X,Y���R���v���C�A���X���[�h�J�n
867     Mov PPlateBackGet_1         '�w�ʔ󂯎����
868     Cnt 1 , 10 , 10
869 '    Cmp Off                     '�R���v���C�A���X���[�h�I��
870 '    ColChk On                   '�Փˌ��mON
871     Ovrd 50
872     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
873     Ovrd 100
874     Accel 100 , 100
875     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '�w�ʃp�l���`�F�b�N
876     If MRtn = 1 Then GoTo *CompPlateGet_2
877     Cnt 0
878     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/2����
879     If M_20# = MNext% Then M_20# = MClear%
880     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
881     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
882     If M_20# = MContinue% Then
883         Mov PPlateBackGet_1
884         Dly 0.3
885         M_Out(12256) = 0            '�{�̃`���b�N��OFF
886         M_Out(12257) = 1            '�{�̃`���b�N�JON
887         Dly 2.0
888     EndIf
889     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
890     *CompPlateGet_2    '
891     '�w�ʔ�u��
892 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
893     ColChk Off
894     Cnt 1 , 100 , 100           '100mm�ߖT�ǉ�(221219����)
895     Mov PPlateBackSet_13        '�w�ʔu�����
896     Cnt 1 , 10 , 10
897 '
898     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '������x�w�ʃp�l���`�F�b�N
899     If MRtn = 1 Then GoTo *CompPlateGet_3
900     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/3����
901     If M_20# = MNext% Then M_20# = MClear%
902     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
903     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
904     If M_20# = MContinue% Then
905         Mov PPlateBackGet_2
906         Mov PPlateBackGet_1
907         M_Out(12256) = 0            '�{�̃`���b�N��OFF
908         M_Out(12257) = 1            '�{�̃`���b�N�JON
909         Dly 2.0
910     EndIf
911     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
912     *CompPlateGet_3
913 '
914 '    ' ���i�����v�����M
915     M_Out(12787) = 1
916 '
917     MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
918     If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
919     If MRtn = 0 Then GoTo *ASSY_ERROR_END
920 '
921     Mov PPlateBackSet_12        '�ܓ���O���_
922     Cnt 0
923     Ovrd 25
924     Accel 25 , 25
925     Mvs PPlateBackSet_11        '�ܓ��ꍞ�ݑO
926     Mvs PPlateBackSet_10        '�ܓ��ꍞ��1
927     Mvs PPlateBackSet_9         '�ܓ��ꍞ��2
928 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
929 '    Cmp Pos, &B001000
930     Cnt 1           '0.1��0.2mm�ߖT�ɕύX(221219����)
931     Mov PPlateBackSet_8         '�o�H1
932     Mov PPlateBackSet_7         '�o�H2
933     Mov PPlateBackSet_6         '�o�H3
934     Mov PPlateBackSet_5         '�o�H4
935     Mov PPlateBackSet_4         '�o�H5
936     Mov PPlateBackSet_3         '�o�H6
937     Mov PPlateBackSet_2         '�o�H7
938     Mov PPlateBackSet_1         '�o�H8
939     Mov PPlateBackSet           '�w�ʔ����ʒu
940 '    Cmp Off
941     Accel 100 , 100
942     Cnt 0
943     Dly 0.1
944     *RE_PLATE_SET
945     M_Out(12256) = 0            '�{�̃`���b�N��OFF
946     M_Out(12257) = 1            '�{�̃`���b�N�JON
947     '
948 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
949     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
950     If MRtn = 1 Then GoTo *CompPlateSet
951     fErrorProcess(11,244,284,0)
952     If M_20# = MNext% Then M_20# = MClear%
953     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
954     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
955     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
956     *CompPlateSet
957     '
958 '
959 '-----�b�艟��-------------------------------------(22/12/14����)��������
960 *RE_BUCK_PUSH
961     M_20# = MClear%
962     Mov PPlateBackPush_2
963 '
964     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
965     M_Out(12256) = 1            '�{�̃`���b�N��ON
966 '
967     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
968 '
969     If MRtn = 1 Then GoTo *CompBuckPushSetting  '����Ȃ牟�������
970 '
971     fErrorProcess(11,245,287,0) '�[�Z���T�[NG���G���[�\��
972         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
973         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
974         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NG�������ꂽ��G���[�G���h��
975         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       '���g���C�������ꂽ�������x����
976 '
977 *CompBuckPushSetting
978 '
979     Mvs PPlateBackPush_1
980     Ovrd 10
981     Mvs PPlateBackPush
982 '    Dly 0.1     '�N�����v������̂ō폜(221219����)
983 '�w�ʃN�����v��������(12/15)
984     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
985     MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
986         If MRtn = 0 Then
987             Mvs PPlateBackPush_1
988             Mov PPlateBackSet_12
989             Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
990         EndIf
991         If MRtn = 0 Then GoTo *ASSY_ERROR_END
992 '�w�ʃN�����v�����܂�(12/15)
993     Ovrd 50                     '20��50�ɕύX(221219����)
994     Mvs PPlateBackPush_1
995 *RE_CHUCK_OPEN
996     M_20# = MClear%
997     M_Out(12256) = 0            '�{�̃`���b�N��OFF
998     M_Out(12257) = 1            '�{�̃`���b�N�JON
999     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1000     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
1001     fErrorProcess(11,244,284,0)
1002         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
1003         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
1004         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NG�������ꂽ��G���[�G���h��
1005         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       '���g���C�������ꂽ�������x����
1006 *CompChuckOpenForBackPush
1007 '-----�b�艟��-------------------------------------(22/12/14����)�����܂�
1008 '
1009     ColChk On
1010     Mov PPlateBackSet_13        '�w�ʔu�����
1011 '    M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
1012     Ovrd 100
1013     '�˂����{���i�N�����v�Œ�҂�(�R�����g�A�E�g221215����)
1014 '    Wait M_In(11891) = 1        '�˂����{2��~4��M
1015 'MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
1016 'If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
1017 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
1018     '
1019     '�˂����{�������ݑ҂�
1020     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)
1021     '
1022     'DVD���J�����
1023     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1024     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1025     '
1026     Mov PMechaGet_3             '�o�H1
1027     Mov PMechaGet_2             'DVD���J�󂯎������_
1028 '    Wait M_In(11272)            '���i�����@Ready
1029 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '���i�����@Ready
1030 '    If MRtn = 0 Then
1031 '        fErrorProcess()         '�G���[����
1032 '    EndIf
1033 '
1034 '    ' ���i�����v�����M(�����ʒu�ύX)
1035     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
1036 '    M_Out(12787) = 1
1037     '    ' ���i���������҂�(�����ύX2/27����)
1038 *RE_FEEDER_READY
1039 '    Wait M_In(11810) = 1
1040 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
1041 If MRtn = 1 Then GoTo *CompFeederReady
1042 '   ' ���i�����v���I��
1043 M_Out(12787) = 0
1044 fErrorProcess(11,289,290,0)                '284��290�ɕύX6/3����
1045 If M_20# = MNext% Then M_20# = MClear%
1046 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1047     Mov PMechaGet_2
1048     Mov PMechaGet_3
1049     Mov PMechaGet_4
1050     Mov PInitialPosition
1051 EndIf
1052 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1053 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1054     ' ���i�����v��
1055 M_Out(12787) = 1
1056 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1057 *CompFeederReady
1058 '    ' ���i�����v���I��
1059     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1060     M_Out(12787) = 0
1061 '
1062     Mov PMechaGet_1             'DVD���J�󂯎����
1063     '
1064     *RE_MECHA_GET_1
1065     '
1066     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1067     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1068     '
1069 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1070     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1071     If MRtn = 1 Then GoTo *CompMechaGet1
1072     Mov PMechaGet_2
1073     Mov PMechaGet_3
1074     Mov PMechaGet_4
1075     fErrorProcess(11,270,284,0)
1076     If M_20# = MNext% Then M_20# = MClear%
1077     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1078     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1079     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1080     Mov PMechaGet_3
1081     Mov PMechaGet_2
1082     Mov PMechaGet_1
1083     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1084     *CompMechaGet1
1085     '
1086     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1087     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1088 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1089     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1090     If MRtn = 1 Then GoTo *CompMechaGet2
1091     Mov PMechaGet_2
1092     Mov PMechaGet_3
1093     Mov PMechaGet_4
1094     fErrorProcess(11,271,284,0)
1095     If M_20# = MNext% Then M_20# = MClear%
1096     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1097     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1098     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1099     Mov PMechaGet_3
1100     Mov PMechaGet_2
1101     Mov PMechaGet_1
1102     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1103     *CompMechaGet2
1104     '
1105     Ovrd 25
1106     Mvs PMechaGet               'DVD���J�󂯎��ʒu
1107     Dly 0.1
1108 '
1109     MRtn = 0
1110     MRtn2 = 0
1111     *RE_MECHA_GET_2
1112     If M_20# = MContinue% Then M_20# = MClear%
1113     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1114     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1115     '
1116 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1117     If MRtn = 1 Then Dly 1.0
1118     If MRtn = 1 Then GoTo *CompMechaGet3
1119     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1120     If M_20# = MNext% Then GoTo *CompMechaGet3
1121     If MRtn = 1 Then GoTo *CompMechaGet3
1122     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1123     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1124     Dly 2.0
1125     Mvs PMechaGet_1
1126     Mov PMechaGet_2
1127     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1128     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1129     Mov PMechaGet_3
1130     Mov PMechaGet_4
1131     fErrorProcess(11,269,294,0) '284��294�ɕύX6/3����
1132     If M_20# = MNext% Then
1133         M_20# = MClear%
1134         MRtn = 1
1135     EndIf
1136     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1137     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1138     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1139     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1140     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1141     Mov PMechaGet_3
1142     Mov PMechaGet_2
1143     Mov PMechaGet_1
1144     Mvs PMechaGet
1145     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1146     *CompMechaGet3
1147     M_20# = MClear%
1148     '
1149 '    Wait M_In(11267) = 1        'DVD���J���o
1150     If MRtn2 = 1 Then GoTo *CompMechaGet4
1151     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVD���J���o
1152     If MRtn2 = 1 Then GoTo *CompMechaGet4
1153     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1154     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1155     Dly 2.0
1156     Mvs PMechaGet_1
1157     Mov PMechaGet_2
1158     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1159     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1160     Mov PMechaGet_3
1161     Mov PMechaGet_4
1162     fErrorProcess(11,273,294,0) '284��294�ɕύX6/3����
1163     If M_20# = MNext% Then
1164         M_20# = MClear%
1165         MRtn2 = 1
1166     EndIf
1167     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1168     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1169     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1170     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1171     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1172     Mov PMechaGet_3
1173     Mov PMechaGet_2
1174     Mov PMechaGet_1
1175     Mvs PMechaGet
1176     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1177     *CompMechaGet4
1178     M_20# = MClear%
1179     Dly 0.5
1180     '
1181     Mvs PMechaGet_1             'DVD���J�󂯎����
1182 '    *RE_MECHA_GET_3
1183     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1184     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1185 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1186     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1187 '    If MRtn = 1 Then GoTo *CompMechaGet5       '�����ʒu�ύX2/11����
1188 '    fErrorProcess(11,272,284,0)
1189 '    If M_20# = MNext% Then M_20# = MClear%
1190 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1191 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1192 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1193 '    *CompMechaGet5
1194     '
1195     If MRtn = 1 Then Ovrd 100
1196     Mov PMechaGet_2             'DVD���J�󂯎������_
1197 '    ' ���i�����v���I��
1198     M_Out(12787) = 0
1199 '    ' ���i�擾�������M(�p���X)
1200     M_Out(12800) = 1 Dly 0.5
1201     Mov PMechaGet_3             '�o�H1
1202     Mov PMechaGet_4             '�o�H2
1203 '
1204     *RE_MECHA_GET_3
1205     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1206     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1207     If MRtn = 1 Then GoTo *CompMechaGet5
1208     fErrorProcess(11,272,284,0)
1209     If M_20# = MNext% Then M_20# = MClear%
1210     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1211     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1212     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1213     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1214     *CompMechaGet5
1215     '
1216     'DVD���J�����u����֒u��
1217 '    Wait M_In(11920) = 0             'BaseUnit6�����u����t���O�m�F(�����ʒu�ύX2/11����)
1218 '
1219 '   ���u���䂪��]�����m�F
1220     *Loop_CW_CCW_1
1221     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1222     *Next_CW_CCW_1
1223     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1          'BaseUnit6�����u����t���O�m�F
1224     *OK_FLG_1
1225 '
1226     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1227     '
1228     'DVD���J�����u����ɒu����Ă��Ȃ����̊m�F(�ǉ���������10/1����)
1229     MRtn = 1
1230     If M_In(11931) = 1 Then          '��]�����̊m�F(CW����)
1231         If M_In(11928) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1232             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1233             Wait M_In(11930) = 1     '���u�����]�҂�
1234             MRtn = 0
1235         EndIf
1236     ElseIf M_In(11930) = 1 Then      '��]�����̊m�F(CCW����)
1237         If M_In(11929) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1238             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1239             Wait M_In(11931) = 1     '���u�����]�҂�
1240             MRtn = 0
1241         EndIf
1242     Else
1243         MRtn = 0
1244     EndIf
1245     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1246     '
1247 *Loop_CW_CCW_S
1248     fnAutoScreenComment(530)    '��ԕ\��[�H���U�̓���I���҂�] 2022/04/26 �n��
1249 'Ver 0.4 �ǉ� -----------------------
1250 '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1251     MRtn = 0
1252     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1253     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1254     If MRtn = 1 Then Dly 0.7
1255     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1256     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1257 'Ver 0.4 �����܂� -------------------
1258 '
1259     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1260     Mov PMechaSet_3             'DVD���J���u�����_1
1261 'Dly 5.0   '�f�o�b�O�p
1262 '
1263 *Loop_CW_CCW_2
1264 'Ver 0.4 �ǉ� -----------------------
1265     '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1266     MRtn = 0
1267     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1268     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1269     If MRtn = 1 Then Dly 0.7
1270     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1271     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1272 'Ver 0.4 �����܂� -------------------
1273 '
1274     Mov PMechaSet_2             'DVD���J���u�����_2
1275 '
1276 '    *Loop_CW_CCW_2  '���u����̏�Ԃ�������x�m�F����(�R�����g�A�E�g2/27����)
1277 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1278 '    *Next_CW_CCW_2
1279 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2         'BaseUnit6�����u����t���O�m�F
1280 '    *OK_FLG_2
1281 ''
1282 '    M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1283 '
1284     '
1285     *RE_MECHA_SET_1
1286     If M_20# = MContinue% Then M_20# = MClear%
1287     Ovrd 25
1288     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1289     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1290 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1291     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1292     If MRtn = 1 Then GoTo *CompMechaSet1
1293     Mov PMechaSet_3
1294     Mov PMechaGet_4
1295     fErrorProcess(11,271,284,0)
1296     If M_20# = MNext% Then M_20# = MClear%
1297     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1298     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1299     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1300     Mov PMechaSet_3
1301     Mov PMechaSet_2
1302     Ovrd 100
1303     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1304     *CompMechaSet1
1305     '
1306     *RE_MECHA_SET_12
1307     Fine 0.05 , P
1308 '    Wait M_In(11920) = 0        'BaseUnit6�����u����t���O�m�F(�R�����g�A�E�g2/27����)
1309 '    M_Out(12912) = 1            '���u����t���O����
1310     If M_In(11931) = 1 Then     '��]�����̊m�F(CW����)(�ǉ������܂�10/1����)
1311         Mov PMechaSet1_1        'DVD���J���u�����1
1312         Ovrd 10
1313         Mvs PMechaSet1          'DVD���J���u���ʒu1
1314         Dly 0.1
1315         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1316         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1317 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1318         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1319         Mvs PMechaSet1_1        'DVD���J���u�����1
1320     ElseIf M_In(11930) = 1 Then '��]�����̊m�F(CCW����)(�ǉ���������10/1����)
1321         Mov PMechaSet2_1        'DVD���J���u�����
1322         Ovrd 10
1323         Mvs PMechaSet2          'DVD���J���u���ʒu2
1324         Dly 0.1
1325         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1326         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1327 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1328         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1329         Mvs PMechaSet2_1        'DVD���J���u�����2
1330     'Else
1331         '�G���[��(���u���䂪����Ȉʒu�ɖ���)
1332     EndIf                       '�ǉ������܂�10/1����
1333     Fine 0 , P
1334     '
1335     If MRtn = 1 Then GoTo *CompMechaSet2
1336     fErrorProcess(11,270,284,0)
1337     If M_20# = MNext% Then M_20# = MClear%
1338     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1339     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1340     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1341     *CompMechaSet2
1342     '
1343     Ovrd 100
1344     *RE_MECHA_SET_2
1345     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1346     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1347 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1348     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1349     If MRtn = 1 Then GoTo *CompMechaSet3
1350     fErrorProcess(11,272,284,0)
1351     If M_20# = MNext% Then M_20# = MClear%
1352     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1353     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1354     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1355     *CompMechaSet3
1356     '
1357     Mov PMechaSet_2             'DVD���J���u�����_2
1358     M_Out(12912) = 0                  '���u����t���O���(�ǉ�10/1����)
1359     '
1360     '�˂����{2�̐��i�����
1361     Mov PProductOnRoboGet_4     '�o�H3����4��
1362     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1363     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1364 '    Wait M_In(11876) = 1        '�˂����{2�����ҋ@����M
1365 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1366 If MRtn = 0 Then Mov PInitialPosition   '"�C�j�V�����ɖ߂铮��"
1367 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1368 '
1369     *RE_ROBO_GET_1
1370 '
1371     M_Out(12259) = 0            'DVD���J�`���b�N�JOFF
1372     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1373     If M_20# = MContinue% Then Dly 0.5
1374 '
1375 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1376     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1377     If MRtn = 1 Then GoTo *CompRoboGet1
1378     fErrorProcess(11,269,284,0)
1379     If M_20# = MNext% Then M_20# = MClear%
1380     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1381     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1382     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1383     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1384     *CompRoboGet1
1385     '
1386 '    Ovrd 50
1387     Mov PProductOnRoboGet_3     '�˂����{���i�������_2����3��
1388 '    Ovrd 20
1389     Mvs PProductOnRoboGet_2     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����2��
1390     Ovrd 20
1391     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1392     Ovrd 10
1393     Mvs PProductOnRoboGet       '�˂����{���i���ʒu
1394     Dly 0.1                     '0.2��0.1�ɕύX(221218����)
1395 '
1396     *RE_ROBO_GET_2
1397 '
1398     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1399     M_Out(12256) = 1            '�{�̃`���b�N��ON
1400 '
1401 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
1402     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
1403     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1404     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1405     M_Out(12257) = 1            '�{�̃`���b�N�JON
1406     Dly 2.0
1407     Mvs PProductOnRoboGet_1
1408     Mvs PProductOnRoboGet_2
1409     Mov PProductOnRoboGet_3
1410     Mov PProductOnRoboGet_4
1411     Mov PInitialPosition
1412     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1413     M_Out(12256) = 1            '�{�̃`���b�N��ON
1414     Dly 1.0
1415     fErrorProcess(11,245,284,0)
1416     If M_20# = MNext% Then MRtn = 1
1417     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1418     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1419     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1420     M_Out(12257) = 1            '�{�̃`���b�N�JON
1421     Dly 2.0
1422     Mov PProductOnRoboGet_4
1423     Mov PProductOnRoboGet_3
1424     Mov PProductOnRoboGet_2
1425     Mvs PProductOnRoboGet_1
1426     Mvs PProductOnRoboGet
1427     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1428     *CompRoboGet2
1429     M_20# = MClear%
1430     '
1431     Dly 0.1                     '0.2��0.1�ɕύX(221218����)
1432     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1433     Ovrd 50
1434     Mvs PProductOnRoboGet_2     '�˂����{���i�������_(9/27�b��R�����g�A�E�g)12/15�R�����g����
1435     Mvs PProductOnRoboGet_3     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����3��
1436     Ovrd 100
1437     Mov PProductOnRoboGet_4     '�o�H3����4��
1438     Cnt 1 , 100 , 100           '10��100�ɕύX(221219����)
1439 '
1440     M_Out(12868) = 1 Dly 0.3    '�˂����{2���슮���𑗐M
1441     *RE_ROBO_GET_3
1442     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1443     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1444 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1445     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1446     If MRtn = 1 Then GoTo *CompRoboGet3
1447     fErrorProcess(11,270,284,0)
1448     If M_20# = MNext% Then M_20# = MClear%
1449     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1450     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1451     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1452     *CompRoboGet3
1453     '
1454     '�p���b�g�֐��i��u��
1455     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1456     Cnt 1 , 10
1457     Mov PProductOnPltSet_1      '�{�̒u���ʒu���
1458     Cnt 0
1459     Ovrd 10
1460     Mvs PProductOnPltSet        '�{�̒u���ʒu
1461     Dly 0.1                     '0.5��0.1�ɕύX(221219����)
1462 '
1463     *RE_PLT_SET
1464 '
1465     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1466     M_Out(12257) = 1            '�{�̃`���b�N�JON
1467 '
1468     Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
1469 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1470     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1471     If MRtn = 1 Then GoTo *CompPltSet
1472     fErrorProcess(11,244,284,0)
1473     If M_20# = MNext% Then M_20# = MClear%
1474     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1475         Mvs PProductOnPltSet_1
1476         Mov PProductOnPltSet_2
1477         Mov PInitialPosition
1478     EndIf
1479     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1480     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1481     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1482     *CompPltSet
1483 '
1484     Mvs PProductOnPltSet_1      '�{�̒u���ʒu���
1485     Ovrd 100
1486     Cnt 1 , 10 , 10             '�ǉ�(221219����)
1487     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1488 '    Mov PInitialPosition        '�C�j�V�����|�W�V����
1489     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
1490     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
1491     Cnt 0                       '�ǉ�(221219����)
1492     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1493     '
1494     '�`�P�b�gID��������
1495     M_20# = MAssyOK%
1496     *ASSY_ERROR_END
1497     *AssyEnd
1498     *fnAssyStart_FEndPosi
1499 FEnd
1500 '
1501 '��fnPiasCheck
1502 ''' <summary>
1503 ''' PIAS�`�P�b�g�Ǎ���
1504 ''' </summary>
1505 ''' <returns>   0 : NG
1506 '''             1 : OK(�Ǎ��݊���)
1507 ''' </returns>
1508 ''' <remarks>
1509 ''' Date   : 2021/07/07 : M.Hayakawa
1510 ''' </remarks>'
1511 Function M% fnPiasCheck
1512     fnPiasCheck = 0
1513     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1514     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1515 '
1516 *RETRY_PIAS
1517     M_20# = MClear%
1518     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1519     '
1520     '�yID�`�P�b�g�ǂݍ��݁z
1521     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1522     MInspGroup%(1) = 1              '����G�ԍ�
1523     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1524 '
1525     '�G���[�̏ꍇ
1526     If MRtn <> 1 Then
1527         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1528         If MRtn <> 1 Then
1529             'D720 -> D1300 �R�s�[�v��
1530             M_Out(12565) = 1
1531             Dly 0.5
1532             M_Out(12565) = 0
1533             '�G���[�����L�q
1534             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1535             'GOT KEY���͑҂�
1536             MKeyNumber = fnKEY_WAIT()
1537             '
1538             Select MKeyNumber
1539                 Case MNext%         '���ւ�I�������ꍇ
1540                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1541                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1542                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1543                     Break
1544                 Case MAbout%        '��~��I�������ꍇ
1545                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1546                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1547                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1548                     Break
1549                 Case MNgProcess%    'NG��I�������ꍇ
1550                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1551                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1552                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1553                     Break
1554                 Case MContinue%     '�p����I�������ꍇ
1555                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1556                     M_20# = MContinue%
1557                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1558                     Break
1559             End Select
1560         EndIf
1561     EndIf
1562 '----------D720 -> D1300 �R�s�[�v��----------
1563     M_Out(12565) = 1
1564     Dly 0.5
1565     M_Out(12565) = 0
1566 '----------�ʐM�m�F������----------
1567     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1568     MRtn = 0                ' ������
1569     M_20# = MClear%         ' ������
1570     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1571     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1572     If MRtn <> 1 Then
1573         If M_20# = MContinue% Then
1574             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1575         Else
1576             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1577         EndIf
1578     EndIf
1579 '----------�H�������m�F----------
1580     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1581     MRtn = 0                ' ������
1582     M_20# = MClear%         ' ������
1583     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1584     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1585     If MRtn <> 1 Then
1586         If M_20# = MContinue% Then
1587             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1588         Else
1589             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1590         EndIf
1591     EndIf
1592     '
1593     fnPiasCheck = 1
1594     *fnPiasCheck_End
1595 FEnd
1596 '
1597 '��fnPCComuCheck
1598 ''' <summary>
1599 ''' PC-PLC�ʐM�`�F�b�N
1600 ''' </summary>
1601 ''' <returns>   0 : NG
1602 '''             1 : OK(�Ǎ��݊���)
1603 ''' </returns>
1604 ''' <remarks>
1605 ''' Date   : 2021/07/07 : M.Hayakawa
1606 ''' </remarks>'
1607 Function M% fnPCComuCheck
1608     fnPCComuCheck = 0
1609     MJudge% = 0                                  '������
1610     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1611     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1612     '
1613     For MStaNo = 0 To 5
1614         '
1615         If M_In(MIN_PIAS_ComOK%) = 1 Then
1616             'PC�ʐMOK(M400)
1617             MJudge% = MOK%
1618             MStaNo = 5
1619             Break
1620         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1621             'toRBT_�ʐM�m�Ftime out
1622             MJudge% = MNG%
1623             MCommentD1001 = 15
1624             MCommentD1002 = 21
1625             MStaNo = 5
1626             Break
1627         Else
1628             'toRBT_�ʐM�m�Ftime out
1629             MJudge% = MNG%
1630             MCommentD1001 = 14
1631             MCommentD1002 = 21
1632             Break
1633         EndIf
1634     Next MStaNo
1635     '
1636     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1637     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1638     '
1639     '�G���[���
1640     If MJudge% <> MOK% Then
1641         M_20# = MClear%     '������
1642         '�G���[�����L�q
1643         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1644         'GOT KEY���͑҂�
1645         MKeyNumber = fnKEY_WAIT()
1646         '
1647         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1648             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1649             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1650             Break
1651         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1652             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1653             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1654             Break
1655         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1656             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1657             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1658             Break
1659         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1660             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1661             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1662             Break
1663         EndIf
1664     Else
1665         'OK�̏ꍇ
1666         fnPCComuCheck = 1
1667     EndIf
1668 FEnd
1669 '
1670 '��fnProcessCheck
1671 ''' <summary>
1672 ''' �H�������m�F
1673 ''' </summary>
1674 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1675 '''             -1�F�O�H������NG  -2�F���H����������
1676 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1677 '''             -5�F���������G���[
1678 ''' </returns>
1679 ''' <remarks>
1680 ''' Date   : 2021/07/07 : M.Hayakawa
1681 ''' </remarks>'
1682 Function M% fnProcessCheck
1683     fnProcessCheck = 0
1684     MJudge% = MNG%      '��UNG���������Ƃ���
1685 '----------�H�������m�F----------
1686     MCommentD1001 = 0   '�R�����g������
1687     For MStaNo = 0 To 5
1688         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1689         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1690         '
1691         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1692             MJudge% = MOK%
1693             fnAutoScreenComment(85)     ' AUTO���
1694             MStaNo = 5
1695             Break
1696         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1697             MFlgLoop% = 0
1698             MJudge% = MNG%
1699             MCommentD1001 = 27
1700             MCommentD1002 = 22
1701             fnAutoScreenComment(94)     ' AUTO���
1702             fnProcessCheck = -2         ' NG��-2��Ԃ�
1703             MStaNo = 5
1704             Break
1705         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1706            MJudge% = MNG%
1707             MCommentD1001 = 31
1708             MCommentD1002 = 22
1709             fnAutoScreenComment(83)     ' AUTO���
1710             fnProcessCheck = -3         ' NG��-3��Ԃ�
1711             MStaNo = 5
1712             Break
1713         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1714             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1715             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1716             MJudge% = MNG%
1717             MCommentD1001 = 32
1718             MCommentD1002 = 22
1719             fnAutoScreenComment(84)     ' AUTO���
1720             fnProcessCheck = -1         ' NG��-1��Ԃ�
1721             Dly 1.0
1722             '�H�������m�FOFF
1723             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1724             Dly 1.0
1725            'MStaNo = 5
1726             Break
1727         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1728             MFlgLoop% = 0
1729             MJudge% = MNG%
1730             MCommentD1001 = 29
1731             MCommentD1002 = 22
1732             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1733             fnProcessCheck = -5         ' NG��-5��Ԃ�
1734             MStaNo = 5
1735             Break
1736         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1737             MJudge% = MNG%
1738             If MCommentD1001 = 32 Then
1739                 '�������Ȃ�
1740             Else
1741                 MCommentD1001 = 26
1742             EndIf
1743             MCommentD1002 = 22
1744             fnProcessCheck = -4         ' NG��-4��Ԃ�
1745             MStaNo = 5
1746             Break
1747         Else
1748             MJudge% = MNG%
1749             MCommentD1001 = 28
1750             MCommentD1002 = 22
1751         EndIf
1752     Next MStaNo
1753     '�H�������m�FOFF
1754     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1755     '�ʉߗ���NG �H�������̏ꍇ
1756     If MJudge% = MPass% Then
1757         M_20# = MPass%
1758     EndIf
1759     '
1760     '�G���[���
1761     If MJudge% <> MOK% Then
1762         M_20# = MClear%     '������
1763         '�G���[�����L�q
1764         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1765         'GOT KEY���͑҂�
1766         MKeyNumber = fnKEY_WAIT()
1767         '
1768         Select MKeyNumber
1769             Case MAbout%        '��~��I�������ꍇ
1770                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1771                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1772                 Break
1773             Case MNext%         '���ւ�I�������ꍇ
1774                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1775                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1776                 Break
1777             Case MContinue%     '�p����I�������ꍇ
1778                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1779                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1780                 Break
1781             Case MNgProcess%    'NG��I�������ꍇ
1782                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1783                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1784                 Break
1785         End Select
1786     Else
1787         fnProcessCheck = 1  ' OK��1��Ԃ�
1788     EndIf
1789 FEnd
1790 '
1791 '��fnPiasWrite
1792 ''' <summary>
1793 ''' Pias �g�����ʏ����ݗv��
1794 ''' </summary>
1795 '''<param name="MFlg%">
1796 '''                 MOK%(1) = �H��������OK��������
1797 '''                 MNG%(0) = �H��������NG��������
1798 '''</param>
1799 '''<returns></returns>
1800 ''' <remarks>
1801 ''' Date   : 2021/07/07 : M.Hayakawa
1802 ''' </remarks>'
1803 Function M% fnPiasWrite(ByVal MFlg%)
1804       fnPiasWrite = 0
1805 *RETRY_PIASWRITE
1806     '
1807     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1808    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1809     If MFlg% = MOK% Then
1810         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1811     Else
1812         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1813     EndIf
1814     Dly 0.1                  '�O�̂���
1815     '
1816     'Pias�֏����݊J�n M305 -> ON
1817     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1818     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1819     '
1820     MJudge% = MNG%
1821     '
1822     For MStaNo = 0 To 5
1823         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1824             MJudge% = MOK%
1825             'MRet = fnAutoScreenComment(85)  'AUTO���
1826             MStaNo = 5
1827             Break
1828         '
1829         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1830             MJudge% = MNG%
1831             'MRet = fnAutoScreenComment(85)  'AUTO���
1832            MCommentD1001 = 34
1833            MCommentD1002 = 25
1834             MStaNo = 5
1835             Break
1836         '
1837         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1838             MJudge% = MNG%
1839             'MRet = fnAutoScreenComment(85)  'AUTO���
1840            MCommentD1001 = 35
1841            MCommentD1002 = 25
1842             MStaNo = 5
1843             Break
1844         '
1845         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1846             MJudge% = MNG%
1847             'MRet = fnAutoScreenComment(85)  'AUTO���
1848            MCommentD1001 = 36
1849            MCommentD1002 = 25
1850             MStaNo = 5
1851             Break
1852         '
1853         Else
1854             MJudge% = MNG%
1855            MCommentD1001 = 42
1856            MCommentD1002 = 25
1857         '
1858         EndIf
1859         '
1860     Next MStaNo
1861     '
1862     'Pias�֏����݊J�n M305 -> OfF
1863     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1864     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1865     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1866     '
1867     '
1868     '�ʉߗ���NG �H�������̏ꍇ
1869     If MJudge% = MPass% Then
1870         M_20# = MPass%
1871     EndIf
1872     '
1873    M_20# = MClear%     '������
1874     '
1875     '�G���[���
1876     If MJudge% < MOK% Then
1877     '
1878 '�c���Ă���������ł͎g�p���Ȃ����x��
1879 *RETRY_ERR_WRITE
1880         M_20# = MClear%     '������
1881         '�G���[�����L�q
1882         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1883         'GOT KEY���͑҂�
1884         MKeyNumber = fnKEY_WAIT()
1885         '
1886         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1887             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1888            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1889             Break
1890         '
1891         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1892             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1893             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1894         '
1895         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1896             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1897             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1898         '
1899         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1900             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1901            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1902             Break
1903         '
1904         EndIf
1905         '
1906         If M_20# = MClear% Then *RETRY_ERR_WRITE
1907         '
1908     EndIf
1909     '
1910     If M_20# = MContinue% Then *RETRY_PIASWRITE
1911     '
1912     fnPiasWrite = 1
1913     '
1914 FEnd
1915 '
1916 '��fnPCBNumberCheck
1917 ''' <summary>
1918 ''' Pias ��ԍ��ƍ��v��
1919 ''' </summary>
1920 '''<param name="%"></param>
1921 '''<param name="%"></param>
1922 '''<returns></returns>
1923 ''' <remarks>
1924 ''' Date   : 2021/07/07 : M.Hayakawa
1925 ''' </remarks>'
1926 Function M% fnPCBNumberCheck
1927       fnPCBNumberCheck = 0
1928     '
1929 *RETRY_PCBCHECK
1930     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1931     'Pias�֊�ƍ��J�n M310 -> ON
1932     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1933     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1934     '
1935     MJudge% = MNG%
1936     '
1937     For MStaNo = 0 To 5
1938         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1939             MJudge% = MOK%
1940             fnAutoScreenComment(96)  'AUTO���
1941             MStaNo = 5
1942             Break
1943         '
1944         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1945             MJudge% = MNG%
1946             fnAutoScreenComment(97)  'AUTO���
1947             MCommentD1001 = 37
1948             MCommentD1002 = 25
1949             MStaNo = 5
1950             Break
1951         '
1952         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1953             MJudge% = MNG%
1954             fnAutoScreenComment(98)  'AUTO���
1955             MCommentD1001 = 38
1956             MCommentD1002 = 25
1957             MStaNo = 5
1958             Break
1959         '
1960         ElseIf M_In(11580) = 1 Then                         'time out
1961             MJudge% = MNG%
1962             fnAutoScreenComment(99)  'AUTO���
1963             MCommentD1001 = 39
1964             MCommentD1002 = 25
1965             MStaNo = 5
1966             Break
1967         '
1968         Else
1969             MJudge% = MNG%
1970            MCommentD1001 = 41
1971            MCommentD1002 = 25
1972         '
1973         EndIf
1974         '
1975     Next MStaNo
1976     '
1977     'Pias�֊�ƍ��J�n M310 -> OfF
1978     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1979     '
1980     '
1981     '�ʉߗ���NG �H�������̏ꍇ
1982     If MJudge% = MPass% Then
1983         M_20# = MPass%
1984     EndIf
1985     '
1986    M_20# = MClear%     '������
1987     '
1988     '�G���[���
1989     If MJudge% < MOK% Then
1990     '
1991 '�c���Ă���������ł͎g�p���Ȃ����x��
1992 *RETRY_ERR_PCBNUMBER
1993         M_20# = MClear%     '������
1994         '�G���[�����L�q
1995         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1996         'GOT KEY���͑҂�
1997         MKeyNumber = fnKEY_WAIT()
1998         '
1999         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2000             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2001             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2002             Break
2003         '
2004         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2005             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2006             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2007         '
2008         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2009             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2010             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2011         '
2012         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2013             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2014             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2015             Break
2016         '
2017         EndIf
2018         '
2019         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
2020         '
2021     EndIf
2022     '
2023     If M_20# = MContinue% Then *RETRY_PCBCHECK
2024 FEnd
2025 '
2026 '��ScrewTight_S2
2027 ''' <summary>
2028 ''' �˂����߂��s��
2029 ''' </summary>
2030 '''<param name="PScrewPos()">
2031 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
2032 '''             PScrewPos(2)    �F�˂����߉��_
2033 '''             PScrewPos(10)   �F�˂����ߏI������
2034 '''</param>
2035 '''<returns>����
2036 '''         0=�ُ�I���A1=����I��
2037 '''</returns>
2038 ''' <remarks>
2039 ''' Date   : 2021/07/07 : M.Hayakawa
2040 ''' </remarks>'
2041 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
2042     ScrewTight_S2 = 0
2043     MOKNGFlg = 0
2044     Ovrd 100
2045     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2046     ' �b��
2047     Ovrd 5
2048     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
2049 '    Ovrd MOvrdA
2050     '�b��}�X�N
2051 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
2052 '    Dly 0.1
2053 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
2054 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
2055 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
2056     ' �b��ړ��̂�
2057     Mvs PScrewPosition(10)
2058 '    '
2059 '    Dly 0.1
2060 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2061 '    Wait M_In(11584)=1          '����/�G���[���o
2062 '    Dly 0.1
2063 '    Spd M_NSpd
2064 '    '
2065 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
2066 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2067 '        Dly 0.1
2068 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2069 '        Dly 0.1
2070 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2071 '        Dly 0.1
2072 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
2073 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2074 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2075 '        MOKNGFlg = -1
2076 '        ScrewTight_S2 = 0
2077 '    Else
2078 '        Wait M_In(X29_Driver)=1 ' ���튮����
2079 '        Dly 0.1
2080 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2081 '        Dly 0.1
2082 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
2083 '        Dly 0.1
2084 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2085 '        Dly 0.1
2086 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2087 '        ScrewTight_S2 = 1
2088 '    EndIf
2089 ' �b��
2090     Ovrd 10
2091     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2092     Ovrd 100
2093 FEnd
2094 '
2095 '��ScrewGet_S3
2096 ''' <summary>
2097 ''' �˂������@����˂��𓾂�
2098 ''' </summary>
2099 '''<param name="%"></param>
2100 '''         PScrewPos(1)    �F�˂�������̂˂����
2101 '''         PScrewPos(2)    �F�˂���������_
2102 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2103 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2104 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2105 '''<returns>����
2106 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
2107 '''</returns>
2108 ''' <remarks>
2109 ''' Date   : 2021/07/07 : M.Hayakawa
2110 ''' </remarks>'
2111 Function M% ScrewGet_S3(ByVal PScrewPosition())
2112     ScrewGet_S3 = 0
2113     MMScrewJudge% = 0
2114     '�˂������평������G���[�`�F�b�N
2115 ' ���b��폜
2116 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
2117 '    Ovrd 100
2118 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
2119 '        Ovrd 30
2120 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
2121 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
2122 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
2123 '        'NG�Ƃ��Ă����̊֐����甲����
2124 '        ScrewGet_S3 = -1
2125 '        MMScrewJudge% = 1
2126 '        MCommentD1001 = 61
2127 '    EndIf
2128 '    If ScrewGet_S3 = 0 Then
2129 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
2130 '        MMScrewJudge% = 0 'MMScrewJudge������������
2131 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2132 '        If MRtn = 0 Then
2133 '            Ovrd 30
2134 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
2135 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
2136 '            MMScrewJudge% = 2
2137 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
2138 '            MCnt% = 2   '2��ݒ�
2139 '            MCommentD1001 = 62
2140 '        EndIf
2141 '        If MMScrewJudge% = 2 Then
2142 '            ScrewGet_S3 = -2
2143 '        EndIf
2144 '    EndIf
2145 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2146 '    If MMScrewJudge% = 2 Then
2147 '        ScrewGet_S3 = -2
2148 '    EndIf
2149     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2150     Ovrd 100
2151     Spd M_NSpd
2152     If MMScrewJudge% = 0 Then
2153         ScrewGet_S3 = 0
2154         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2155         MScrewCnt% = 0
2156         MFinCnt% = 2
2157 '        For MCnt% = 0 To MFinCnt%
2158             Mov PScrewPosition(2)        ' �˂������@���_
2159             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2160             Ovrd 80
2161             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2162             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2163             Mvs PScrewPosition(10), 1.2
2164             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
2165             '�r�b�g��]
2166             M_Out(Y60_Driver)=1
2167             Dly 0.2
2168             '
2169             Ovrd 100
2170             JOvrd M_NJovrd
2171             Spd M_NSpd
2172             '�l�W�z���m�F�ʒu�ړ�
2173             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2174             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2175             '�r�b�g��]��~
2176             'M_Out(Y60_Driver)=0
2177             '
2178             '1�b�ԃl�W�z���m�F
2179 ' �ȉ��b��폜
2180 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2181 '            'MRtn = 0'�����G���[
2182 '            '�z���G���[�̏ꍇ
2183 '            '�l�W���˂����Y�ɖ߂�
2184 '            If MRtn = 0 Then
2185 '                Ovrd 30
2186 '                '�r�b�g��]��~
2187 '                M_Out(Y60_Driver)=0
2188 '                '�l�W�����@���
2189 '                Mvs PScrewPos(1)
2190 '                '�X�ɏ��
2191 '                Mov PScrewPos(1), -75
2192 '                '�l�W�̂Ĉʒu
2193 '                Mov PScrewFeedS021
2194 '                '�z��OFF
2195 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
2196 '                Dly 0.2
2197 '                '�j��ON
2198 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2199 '                '�r�b�g��]
2200 '                M_Out(Y61_Driver)=1
2201 '                Dly 0.5
2202 '                '
2203 '                Ovrd 100
2204 '                JOvrd M_NJovrd
2205 '                Spd M_NSpd
2206 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2207 '                Mov PScrewFeedS021, 10
2208 '                Mov PScrewFeedS021
2209 '                Dly 0.1
2210 '                Mov PScrewFeedS021, 10
2211 '                Mov PScrewFeedS021
2212 '                '
2213 '                '�l�W�����҂�
2214 '                '�r�b�g��]��~
2215 '                M_Out(Y61_Driver)=0
2216 '                Dly 0.1
2217 '                '�j��OFF
2218 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2219 '                '
2220 '                '
2221 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2222 '                Mov PScrewPos(1), -75
2223 '                Ovrd 100
2224 '                Spd M_NSpd
2225 '                '�l�W�����@���
2226 '                Mvs PScrewPos(1)
2227 '                '
2228 '                ScrewGet_S3 = -3
2229 '                Break
2230 '                '
2231 '            Else
2232 '                MCnt% = MFinCnt%
2233 '                ScrewGet_S3 = 0
2234 '            EndIf
2235 '        Next  MCnt%
2236         '
2237         Ovrd 100
2238         Spd M_NSpd
2239         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2240         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2241         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2242         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2243         '������x�z���m�F
2244 ' �ȉ��b��폜
2245 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2246 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2247 '            MCommentD1001 = 94
2248 '            MCommentD1002 = 95
2249 '            ScrewGet_S3 = -3
2250 '        EndIf
2251 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2252 '            ScrewGet_S3 = 1
2253 '        EndIf
2254 '        Break
2255     Else
2256         'M�l�W
2257         If MMScrewJudge% = 2 Then
2258             ScrewGet_S3 = -2
2259         EndIf
2260     EndIf
2261 FEnd
2262 '
2263 '��fnKEY_WAIT()
2264 ''' <summary>
2265 ''' GOT����̃L�[���͑҂�
2266 ''' </summary>
2267 '''<returns>1�F��~    2�F����
2268 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2269 '''         5�FNG
2270 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2271 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2272 '''</returns>
2273 ''' <remarks>
2274 ''' Date   : 2021/07/07 : M.Hayakawa
2275 ''' </remarks>'
2276 Function M% fnKEY_WAIT()
2277     fnKEY_WAIT = 0
2278     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2279     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2280     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2281     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2282     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2283     Dly 0.2
2284     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2285     MLocalLoopFlg=1
2286     While MLocalLoopFlg=1
2287         If M_In(11345) = 1 Then         '��~   M5345
2288             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2289             fnKEY_WAIT = 1
2290             MLocalLoopFlg=-1
2291             Break
2292         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2293             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2294             fnKEY_WAIT = 2
2295             MLocalLoopFlg=-1
2296             Break
2297         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2298             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2299             fnKEY_WAIT = 3
2300             MLocalLoopFlg=-1
2301             Break
2302         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2303             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2304             fnKEY_WAIT = 4
2305             MLocalLoopFlg=-1
2306             Break
2307         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2308             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2309             fnKEY_WAIT = 5
2310             MLocalLoopFlg=-1
2311             Break
2312             '
2313         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2314             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2315             fnKEY_WAIT = MRobotInit1%
2316             MLocalLoopFlg=-1
2317             Break
2318             '
2319         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2320             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2321             fnKEY_WAIT = MRobotInit2%
2322             MLocalLoopFlg=-1
2323             Break
2324             '
2325         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2326             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2327             fnKEY_WAIT = MRobotInit3%
2328             MLocalLoopFlg=-1
2329             Break
2330             '
2331         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2332             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2333             fnKEY_WAIT = MRobotInit4%
2334             MLocalLoopFlg=-1
2335             Break
2336             '
2337         Else
2338         EndIf
2339     WEnd
2340     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2341     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2342 FEnd
2343 '
2344 '�� fnAUTO_CTL
2345 ''' <summary>
2346 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2347 ''' </summary>
2348 ''' <remarks>
2349 ''' Date   : 2021/07/07 : M.Hayakawa
2350 ''' </remarks>
2351 Function M% fnAUTO_CTL
2352     fnAUTO_CTL = 0
2353     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2354     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2355     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2356     '
2357     If M_Svo=0 Then             '�T�[�{ON�m�F
2358         Servo On
2359     EndIf
2360     Wait M_Svo=1
2361 FEnd
2362 '
2363 '�� fnWindScreenOpen
2364 ''' <summary>
2365 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2366 ''' </summary>
2367 '''<param name="%"></param>
2368 '''<param name="%"></param>
2369 '''<param name="%"></param>
2370 '''<param name="%"></param>
2371 ''' <remarks>
2372 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2373 ''' MWindReSet = 0     ��ʔ�\��
2374 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2375 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2376 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2377 ''' Date   : 2021/07/07 : M.Hayakawa
2378 ''' </remarks>
2379 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2380     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2381         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2382     EndIf
2383     '
2384     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2385         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2386     EndIf
2387     '
2388     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2389        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2390     EndIf
2391     '
2392     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2393     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2394     Dly 0.5
2395     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2396 FEnd
2397 '
2398 '��FnCtlValue2
2399 ''' <summary>
2400 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2401 ''' </summary>
2402 ''' <param name="MCtlNo%"></param>
2403 ''' <remarks>
2404 ''' Date : 2022/04/28 �n��
2405 ''' </remarks>
2406 '''
2407 '''  1�F������       �{�P
2408 '''  2�F�g���n�j��   �{�P
2409 '''  3�F�g���m�f��   �{�P (���g�p)
2410 '''  4�F�z���G���[�� �{�P
2411 ''' 99�F�Ǐ��J�n�M�� OFF
2412 '''
2413 Function M% FnCtlValue2(ByVal MCtlNo%)
2414     FnCtlValue2 = 1
2415     Select MCtlNo%
2416         Case 1        '�������{�P
2417             M_Out(12569) = 0             '�����݊J�n�M��OFF
2418             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2419             MInputQty = M_In16(11600)    '��������M
2420             MInputQty = MInputQty + 1    '�������{�P
2421             M_Out16(12592) = MInputQty   '���������M
2422             M_Out(12569) = 1             '�����݊J�n�M��ON
2423             Break
2424             '
2425         Case 2        '�g���n�j���{�P
2426             M_Out(12569) = 0             '�����݊J�n�M��OFF
2427             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2428             MAssyOkQty = M_In16(11616)   '�g��OK����M
2429             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2430             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2431             M_Out(12569) = 1             '�����݊J�n�M��ON
2432             Break
2433             '
2434         Case 4        '�z���G���[���{�P
2435             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2436             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2437             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2438             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2439             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2440             M_Out(12569) = 1                       '�����݊J�n�M��ON
2441             Break
2442             '
2443         Case 99        '�Ǐ��J�n�M��OFF
2444             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2445             M_Out(12569) = 0        '�����݊J�n�M��OFF
2446             Break
2447             '
2448     End Select
2449     Exit Function
2450 FEnd
2451 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2452 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2453 '-------------------------------------------------------------------------------
2454 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2455 '   ����
2456 '       PInspPos()      �F�����ʒu
2457 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2458 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2459 '       MInspCnt%       �F�����ʒu��
2460 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2461 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2462 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2463 '   �߂�l�F����
2464 '       0=�ُ�I���A1=����I��
2465 '
2466 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2467 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2468 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2469 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2470 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2471 '-------------------------------------------------------------------------------
2472     '----- �����ݒ� -----
2473     Cnt 0                                                           '�ړ�����������(�����l=0)
2474     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2475 '    Cnt 1,0.1,0.1
2476     '�ϐ��錾�E������
2477     Def Inte MNum                                                   '�����ԍ�(������1�`)
2478     MNum% = 1                                                       '�����ԍ������l�ݒ�
2479     Def Inte MEndFlg                                                '�����I���t���O
2480     MEndFlg% = 0
2481     '
2482     '����G�ԍ��ݒ�v���E�������s�v��off
2483     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2484     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2485     '�G���[�ԍ��N���A
2486     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2487     M_Out16(MOUT_InspErrNum) = MInspErrNum
2488     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2489     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2490     '
2491     'Insight Ready check?
2492     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2493         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2494         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2495         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2496         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2497         Exit Function
2498     EndIf
2499     '
2500     '�����ʒu���m�F
2501     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2502         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2503         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2504         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2505         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2506         Exit Function
2507     EndIf
2508     '
2509     '
2510     '
2511     '----- ���C������ -----
2512     '�ݒ肳�ꂽ�����ʒu�����̌������s
2513     While( MEndFlg% = 0 )
2514         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2515         MSetGrNumRetryExitFlg = 0
2516         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2517         While( MSetGrNumRetryExitFlg = 0 )
2518         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2519             '
2520             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2521             '
2522             '----- �����O���[�v�ԍ��ݒ� -----
2523             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2524             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2525             '
2526             '�����ʒu�ֈړ��E�ړ������҂�
2527             Mvs PInspPos( MNum% )                                       '�ړ�
2528             Dly 0.05                                                    '�ړ�������Delay
2529             '
2530             '�����O���[�v�ԍ��ݒ�I���m�F
2531             M_Timer(1) = 0
2532             MExitFlg = 0
2533             While( MExitFlg = 0 )
2534                 '����G�ݒ萳��I��?
2535                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2536                     MExitFlg = 1
2537                 '
2538                 '����G�ݒ�ُ�I��?
2539                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2540                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2541                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2542                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2543                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2544                     EndIf
2545                     MExitFlg = 1
2546                 '
2547                 'timeout�`�F�b�N
2548                 ElseIf 1000 < M_Timer(1) Then
2549                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2550                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2551                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2552                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2553                     EndIf
2554                     MExitFlg = 1
2555                 EndIf
2556             WEnd
2557             '
2558             '����G�ԍ��ݒ�v��off
2559             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2560             '
2561             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2562             'NG�Ȃ���Δ�����
2563             If MCurrentStepErr = 0 Then
2564                 MSetGrNumRetryExitFlg = 1
2565             Else
2566                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2567                 If MSetGrNumRetryCnt = 0 Then
2568                     MSetGrNumRetryExitFlg = 1
2569                 Else
2570                     'Retry�ց@���̑O��Delay
2571                     Dly 0.5
2572                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2573                 EndIf
2574             EndIf
2575             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2576             '
2577         WEnd
2578         '
2579         '
2580         '
2581         '----- �������s -----
2582         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2583             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2584                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2585                 MInspRetryExitFlg = 0
2586                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2587                 While( MInspRetryExitFlg = 0 )
2588                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2589                     '
2590                     '���������m�F
2591                     MRetryCnt = MRetryCnt - 1
2592                     M_Timer(1) = 0
2593                     MExitFlg = 0
2594                     While( MExitFlg = 0 )
2595                     '���������҂�
2596                         '����OK�I��?
2597                         If M_In( MIN_IS_InspOK% ) = 1  Then
2598                             MJudgeOKFlg = 1                         '����OK�t���OON
2599                             MExitFlg = 1
2600                         '
2601                         '����NG�I��?
2602                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2603                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2604                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2605                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2606                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2607                                 EndIf
2608                             EndIf
2609                             MExitFlg = 1
2610                         '
2611                         '�����ُ�I��(IS timeout)?
2612                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2613                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2614                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2615                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2616                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2617                                 EndIf
2618                             EndIf
2619                             MExitFlg = 1
2620                         '
2621                         'timeout�`�F�b�N
2622                         ElseIf 3000 < M_Timer(1) Then
2623                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2624                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2625                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2626                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2627                                 EndIf
2628                             EndIf
2629                             MExitFlg = 1
2630                         EndIf
2631                     WEnd
2632                     '
2633                     '�����J�n�v��off
2634                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2635                     '
2636                     'OK�Ȃ甲����
2637                     If MJudgeOKFlg = 1 Then
2638                         MInspRetryExitFlg = 1
2639                     Else
2640                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2641                         If MRetryCnt = 0 Then
2642                             MInspRetryExitFlg = 1
2643                         Else
2644                             'Retry�ց@���̑O��Delay
2645                             Dly 0.3
2646                         EndIf
2647                     EndIf
2648                     '
2649                 WEnd
2650             EndIf
2651         EndIf
2652         '
2653         '
2654         '
2655         MNum% = MNum% + 1                                           '����Step+1
2656         '�����I���m�F�@�����I���t���O�Z�b�g
2657         If (MInspCnt% < MNum% ) Then
2658             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2659         EndIf
2660         'NG���������s������
2661         If MInspErrNum <> 0 Then                                    'NG����?
2662             If MNgContinue% <> 1 Then                               'NG���s?
2663                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2664             EndIf
2665         EndIf
2666     WEnd
2667     '
2668     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2669     If 0 < MZAxis% Then
2670         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2671         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2672         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2673     EndIf
2674     Fine 0 , P
2675     '
2676     '�߂�l�ݒ�
2677     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2678         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2679     Else
2680         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2681         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2682         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2683     EndIf
2684     '
2685 FEnd
2686 '
2687 ' ��ISInspection
2688 ''' <summary>
2689 ''' Insight�ɂ��摜�����������s
2690 ''' </summary>
2691 '''<param name="PInspPos()">�����ʒu</param>
2692 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2693 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2694 '''<param name="MInspCnt%">�����ʒu��</param>
2695 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2696 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2697 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2698 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2699 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2700 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2701 ''' <remarks>
2702 ''' Date   : 2021/07/07 : M.Hayakawa
2703 ''' </remarks>
2704 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2705 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2706 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2707 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2708 '    EndIf
2709 ''
2710 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2711 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2712 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2713 '    Def Inte MEndFlg                                            '�����I���t���O
2714 '    MEndFlg% = 0
2715 '    '
2716 '    '�G���[�ԍ��N���A
2717 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2718 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2719 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2720 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2721 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2722 '    '
2723 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2724 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2725 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2726 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2727 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2728 ''
2729 '    EndIf
2730 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2731 '    '
2732 '    '�����ʒu���m�F
2733 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2734 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2735 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2736 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2737 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2738 ''
2739 '    EndIf
2740 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2741 '    '
2742 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2743 '    While( MEndFlg% = 0 )
2744 '        '�����I���m�F�@�����I���t���O�Z�b�g
2745 '        If (MInspCnt% < MNum% ) Then
2746 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2747 '        EndIf
2748 '        '
2749 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2750 '        If MEndFlg% = 0 Then
2751 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2752 '        EndIf
2753 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2754 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2755 '        '�^�X�N�@����G�ݒ�t���O���n��
2756 '        If MEndFlg% = 0 Then
2757 '            If 0 < MInspGrNum%(MNum%) Then
2758 '                M_03# = 1
2759 '            Else
2760 '                M_03# = 0
2761 '            EndIf
2762 '        Else
2763 '            M_03# = 0
2764 '        EndIf
2765 '        '�^�X�N�@�������ʊm�F�t���O���n��
2766 '        If 1 < MNum% Then
2767 '            If 0 < MInspGrNum%(MNum%-1) Then
2768 '                M_04# = 1
2769 '            Else
2770 '                M_04# = 0
2771 '            EndIf
2772 '        Else
2773 '            M_04# = 0
2774 '        EndIf
2775 '        '
2776 '        '�^�X�N�����J�n
2777 '        M_00# = 1                                               'TASK�����J�n
2778 '        '�^�X�N�����J�n�m�F
2779 '        M_Timer(1) = 0
2780 '        MExitFlg = 0
2781 '        While( MExitFlg = 0 )
2782 '            '�����J�n�����m�F
2783 '            If M_00# = 0 And M_10# = 8 Then
2784 '                MExitFlg = 1
2785 '            EndIf
2786 '            'timeout�`�F�b�N
2787 '            If 2000 < M_Timer(1) Then
2788 '                If MNgContinue% = 1 Then                        'NG���s?
2789 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2790 '                Else
2791 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2792 '                EndIf
2793 '                MExitFlg = 1
2794 '            EndIf
2795 '        WEnd
2796 '        '
2797 '        '�����ʒu�ֈړ��E�ړ������҂�
2798 '        If 0 = MInspErrNum Then
2799 '            If MEndFlg% = 0 Then
2800 '                Mvs PInspPos( MNum% )                           '�ړ�
2801 '            EndIf
2802 '        EndIf
2803 '        '
2804 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2805 '        If 0 = MInspErrNum Then
2806 '            M_Timer(1) = 0
2807 '            MExitFlg = 0
2808 '            While( MExitFlg = 0 )
2809 '                '���������҂��i����I���j
2810 '                If M_10# = 1 Then
2811 '                    MExitFlg = 1
2812 '                EndIf
2813 '                '���������҂��i�ُ�I���j
2814 '                If M_10# = 0 Then
2815 '                    If MNgContinue% = 1 Then                    'NG���s?
2816 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2817 '                    Else
2818 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2819 '                    EndIf
2820 '                    MExitFlg = 1
2821 '                EndIf
2822 '                'timeout�`�F�b�N
2823 '                If 5000 < M_Timer(1) Then
2824 '                    If MNgContinue% = 1 Then                    'NG���s?
2825 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2826 '                    Else
2827 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2828 '                    EndIf
2829 '                    MExitFlg = 1
2830 '                EndIf
2831 '            WEnd
2832 '        EndIf
2833 '        '
2834 '        '�������ʊm�F
2835 '        If 0 = MInspErrNum Then
2836 '            If 1 < MNum% Then
2837 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2838 '                    If M_11# = 2 Then                           '����NG?
2839 '                        If MNgContinue% = 1 Then                'NG���s?
2840 '                            If MInspNGStepNum = 0 Then          'NG������?
2841 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2842 '                            EndIf
2843 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2844 '                        Else
2845 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2846 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2847 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2848 '                        EndIf
2849 '                   EndIf
2850 '                EndIf
2851 '            EndIf
2852 '        EndIf
2853 '        '
2854 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2855 '        If 0 <> MInspErrNum Then
2856 '            MEndFlg% = 1
2857 '        EndIf
2858 '        '
2859 '        '�������s�A�捞�����҂�
2860 '        If 0 = MInspErrNum Then
2861 '            If MEndFlg% = 0 Then
2862 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2863 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2864 '                    '�捞�����m�F
2865 '                    M_Timer(1) = 0
2866 '                    MExitFlg = 0
2867 '                    While( MExitFlg = 0 )
2868 '                        '���������҂�
2869 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2870 '                            MExitFlg = 1
2871 '                        EndIf
2872 '                        'timeout�`�F�b�N
2873 '                        If 2000 < M_Timer(1) Then
2874 '                            If MNgContinue% = 1 Then            'NG���s?
2875 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2876 '                            Else
2877 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2878 '                            EndIf
2879 '                            MExitFlg = 1
2880 '                        EndIf
2881 '                    WEnd
2882 '                EndIf
2883 '                '
2884 '            EndIf
2885 '        EndIf
2886 '        MNum% = MNum% + 1
2887 '    WEnd
2888 '    '
2889 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2890 '    If 0 < MZAxis% Then
2891 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2892 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2893 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2894 '    EndIf
2895 '    '
2896 '    'NG���s������
2897 '    If MNgContinue% = 1 Then                                    'NG���s?
2898 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2899 '    EndIf
2900 '    '
2901 '    '�߂�l�ݒ�
2902 '    If MInspErrNum = 0 Then
2903 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2904 '    Else
2905 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2906 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2907 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2908 '    EndIf
2909 '    '
2910 '*ISInspection_End
2911 'FEnd
2912 '
2913 '��InitialZoneB
2914 ''' <summary>
2915 ''' ����~��̕��A����
2916 ''' 1)���ޔ��@Z������Ɉړ�
2917 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2918 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2919 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2920 ''' </summary>
2921 ''' <remarks>
2922 ''' Date : 2022/03/23 : N.Watanabe
2923 ''' </remarks>
2924 Function V fnInitialZoneB()
2925     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2926 '
2927 '�p�����[�^
2928     Ovrd 5
2929 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2930 '    Cmp Pos, &B100011
2931 '
2932 '���A����J�n
2933 '
2934 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2935 *RecoveryChuckOpen
2936     PActive = P_Curr          '���݈ʒu���擾
2937     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2938 'PProductOnRoboSet(�˂����{���i�u���ʒu)�́A�`���b�N���
2939     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2940         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2941             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2942                 MRecoveryChuckOpen = 1
2943             EndIf
2944         EndIf
2945     EndIf
2946 'PProductOnRoboGet(�˂����{���i���ʒu)�́A�`���b�N���
2947     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2948         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2949             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2950                 MRecoveryChuckOpen = 1
2951             EndIf
2952         EndIf
2953     EndIf
2954 '
2955     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2956     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2957     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2958 '
2959     M_20# = 0                                  'KEY���͏�����
2960     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2961     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2962     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2963 '
2964     fErrorProcess(11,244,284,0)
2965     If M_20# = MNext% Then M_20# = MClear%
2966     If M_20# = MAbout% Then GoTo *RecoveryEnd
2967     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2968     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2969 '
2970     *RecoveryChuckOpenEnd
2971 '
2972 '�w�ʔ��
2973 'PPlateBackSet�`PPlateBackSet_6�̃G���A�ɂ���Ƃ��́A�{�̃`���b�N�J��
2974 '�EPPlateBackSet_6         '�o�H6
2975 '�EPPlateBackSet_5         '�o�H7
2976 '�EPPlateBackSet_4         '�o�H8
2977 '�EPPlateBackSet_3         '�o�H9
2978 '�EPPlateBackSet_2         '�o�H10
2979 '�EPPlateBackSet_1         '�o�H11
2980 '�EPPlateBackSet           '�w�ʔu���ʒu
2981 '��L�V�_�̂w���W�E�x���W�E�y���W��J6�������LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2982     PActive = P_Curr                    '���݈ʒu���擾
2983     JActive = J_Curr                    '���݈ʒu���擾
2984     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2985     If (PActive.X >= -35) And (PActive.X <= -5) Then
2986         If (PActive.Y >= 350) And (PActive.Y <= 515) Then
2987             If (PActive.Z >= 480) And (PActive.Z <= 560) Then
2988                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2989                     M_Out(12256) = 0            '�{�̃`���b�N��OFF
2990                     M_Out(12257) = 1            '�{�̃`���b�N�JON
2991                 Dly 1.0
2992                 EndIf
2993             EndIf
2994         EndIf
2995     EndIf
2996 '
2997 '
2998 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2999 '
3000     Ovrd 1
3001 'PProductOnRoboSet(Get)�`PProductOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_2��
3002 '�EPProductOnRoboSet
3003 '�EPProductOnRoboSet_1
3004 '�EPProductOnRoboSet_2
3005 '�EPProductOnRoboGet
3006 '�EPProductOnRoboGet_1
3007 '�EPProductOnRoboGet_2
3008 '��L�U�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
3009     PActive = P_Curr                    '���݈ʒu���擾
3010     JActive = J_Curr                    '���݈ʒu���擾
3011     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
3012     If (PActive.X >= -30) And (PActive.X <= 0) Then
3013         If (PActive.Y >= 380) And (PActive.Y <= 420) Then
3014             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
3015                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3016                     Mvs PProductOnRoboSet_1
3017                     Dly 1.0
3018                     Mvs PProductOnRoboSet_2
3019                     Dly 1.0
3020                     Mov PProductOnRoboSet_3
3021                     Dly 1.0
3022                 EndIf
3023             EndIf
3024         EndIf
3025     EndIf
3026 '
3027 'PProductOnRoboSet(Get)_2�`PProductOnRoboSet(Get)_3�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_3��
3028 '�EPProductOnRoboSet_2
3029 '�EPProductOnRoboSet_3
3030 '�EPProductOnRoboGet_2
3031 '�EPProductOnRoboGet_3
3032 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
3033     PActive = P_Curr                    '���݈ʒu���擾
3034     JActive = J_Curr                    '���݈ʒu���擾
3035     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
3036     If (PActive.X >= -35) And (PActive.X <= 0) Then
3037         If (PActive.Y >= 280) And (PActive.Y <= 400) Then
3038             If (PActive.Z >= 410) And (PActive.Z <= 530) Then
3039                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3040                     Mvs PProductOnRoboSet_3
3041                     Dly 1.0
3042                 EndIf
3043             EndIf
3044         EndIf
3045     EndIf
3046 '
3047     Ovrd 5
3048 '
3049 '���ޔ�
3050     PActive = P_Curr
3051     Pmove = PActive
3052     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
3053     If PActive.X > 550 Then
3054         Pmove.Z =550        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
3055     EndIf
3056     If PActive.Z < Pmove.Z Then
3057         Mvs Pmove
3058     EndIf
3059     Dly 1.0
3060 'J1���ȊO��ޔ��|�W�V�����ֈړ�
3061     JActive = J_Curr
3062     Jmove = JTaihi
3063     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3064     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3065     Mov Jmove
3066     Dly 1.0
3067 'J1���݂̂�ޔ��|�W�V�����ֈړ�
3068     Mov JTaihi
3069     Dly 1.0
3070 '�C�j�V�����|�W�V�����ֈړ�
3071     Mov PInitialPosition
3072     Cmp Off
3073     Ovrd 100
3074 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
3075     If M_In(11856) = 0 Then                 ' ��~���̂�
3076         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
3077         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
3078         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
3079         If MRet = 0 Then
3080         Else
3081             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
3082         EndIf
3083     EndIf
3084     M_Out(12262) = 0            '�ʒu���ߏoOFF
3085     M_Out(12263) = 1            '�ʒu���ߖ�ON
3086     fErrorProcess(11,253,281,0)
3087 *RecoveryEnd
3088     Exit Function
3089 FEnd
3090 '
3091 '
3092 '��fnAutoScreenComment
3093 ''' <summary>
3094 ''' ���C����ʂ̓���󋵕\��
3095 ''' �R�����gD1005�̐ݒ�
3096 ''' </summary>
3097 '''<param name="McommentD1005%">�R�����gID</param>
3098 ''' <remarks>
3099 ''' Date   : 2021/07/07 : M.Hayakawa
3100 ''' </remarks>
3101 Function fnAutoScreenComment(ByVal McommentD1005%)
3102     M_Out16(12576) = McommentD1005%
3103 FEnd
3104 '
3105 '��fnRoboPosChk
3106 ''' <summary>
3107 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
3108 ''' </summary>
3109 '''<param name="MINNumber%">���͔ԍ�</param>
3110 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3111 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3112 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
3113 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
3114 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3115 ''' <remarks>
3116 ''' Date   : 2021/07/07 : M.Hayakawa
3117 ''' </remarks>
3118 Function M% fnRoboPosChk
3119     fnRoboPosChk = 0
3120     MRet = fnStepRead()
3121     '�����ʒu�łȂ��Ɣ��f�����ꍇ
3122     '�E�B���h��ʐ؊���
3123     If MRBTOpeGroupNo > 5 Then
3124         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3125         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3126         Dly 0.2
3127         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3128         Dly 1.5
3129         '
3130         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3131         '
3132         MLoopFlg% = 1
3133         While MLoopFlg% = 1
3134             '
3135             '
3136             MKeyNumber% = fnKEY_WAIT()
3137             Select MKeyNumber%
3138                 Case Is = MAbout%       '��~
3139                     M_20# = MAbout%
3140                     MLoopFlg% = -1
3141                     Break
3142                 Case Is = MNext%        '����
3143                     'MLoopFlg% = -1
3144                     Break
3145                 Case Is = MContinue%    '�p��
3146                     M_20# = MContinue%
3147                     MLoopFlg% = -1
3148                     Break
3149                 Default
3150                     Break
3151             End Select
3152         WEnd
3153     EndIf
3154     '
3155     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3156         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3157         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3158         Select MRBTOpeGroupNo
3159             Case Is = 5                          '�������Ȃ�
3160                 Break
3161             Case Is = 10                         '�����ʒu�֖߂�
3162                 'Mov PTEST001
3163                 Break
3164             Case Is = 15                         '�����ʒu�֖߂�
3165                 'Mov PTEST002
3166                 Dly 0.5
3167                 'Mov PTEST001
3168                 Dly 0.5
3169                 Break
3170             Default
3171                 Break
3172         End Select
3173         '
3174         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3175         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3176         MRBTOpeGroupNo = 5
3177         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3178         Dly 1.0
3179         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3180         fnRoboPosChk = 1                        '�����ʒu������s
3181         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3182     EndIf
3183     Exit Function
3184 FEnd
3185 '
3186 '��frInCheck
3187 ''' <summary>
3188 ''' �Z���T�[IN�`�F�b�N
3189 ''' </summary>
3190 '''<param name="MINNumber%">���͔ԍ�</param>
3191 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3192 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3193 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3194 ''' <remarks>
3195 ''' Date   : 2021/07/07 : M.Hayakawa
3196 ''' </remarks>
3197 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3198     M_Timer(4) = 0
3199     MloopFlg = 0
3200     While MloopFlg = 0
3201         MCrtTime& = M_Timer(4)
3202         If M_In(MINNumber%) = MCMPFLG% Then
3203             MloopFlg = 1
3204             frInCheck = 1
3205         ElseIf MCrtTime& > MTimeCnt& Then
3206             MloopFlg = 1
3207             frInCheck = 0
3208         EndIf
3209     WEnd
3210 FEnd
3211 '-----------------------------------------------
3212 '
3213 '�˂����ߋ@�ʐM�m�F
3214 '
3215 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3216 'fScrewTcomChk = 0�@�F����I��
3217 '          �@ �@ -1 �F�ُ�I��
3218 '-----------------------------------------------
3219 Function M% fScrewTcomChk
3220 *ReCheckScewTcomChk
3221     fScrewTcomChk = 0
3222     '�ʐM�m�F���M
3223     M_Out(MOUT_ScwT_ComChk%) = MOn%
3224     '�ʐM�m�F��M�ҋ@
3225 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3226     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3227     '�ʐM�m�F���M�I��
3228     M_Out(MOUT_ScwT_ComChk%) = MOff%
3229     If MRtn = 0 Then
3230         fScrewTcomChk = -1
3231     EndIf
3232     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3233  '
3234 FEnd
3235 '
3236 '
3237 '-----------------------------------------------
3238 '
3239 '�˂����ߊJ�n���M
3240 '
3241 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3242 'fScrewTStart = 0�@�F����I��
3243 '           �@�@-1 �F�ُ�I��
3244 '-----------------------------------------------
3245 Function M% fScrewTStart
3246     fScrewTStart = 0
3247     nRet% = 0
3248     '�˂����ߊJ�n�ҋ@����M
3249 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3250     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3251     If MRtn = 0 Then nRet% = -1
3252     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
3253     Dly 0.1
3254     '�˂����ߊJ�n��M�𑗐M
3255     M_Out(MOUT_ScwT_ST%) = MOn%
3256     Dly 0.5
3257     'Wait M_In(MTEST_KEY%) = MOn%
3258     '�˂����ߊJ�n���M�I��
3259     M_Out(MOUT_ScwT_ST%) = MOff%
3260     '
3261 *ScrewStartERROR
3262     fScrewTStart = nRet%
3263 FEnd
3264 '
3265 '
3266 '
3267 '-----------------------------------------------
3268 '
3269 '�˂����ߊ�����M
3270 '
3271 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3272 'fScewTFinish = 0�@�F����I��
3273 '          �@ �@-1 �F�ُ�I��
3274 '-----------------------------------------------
3275 Function M% fScewTFinish
3276 *ReCheckScewTFinish
3277     fScewTFinish = 0
3278     '�˂����ߊ����ҋ@����M
3279 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3280     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3281     If MRtn = 0 Then
3282         fScewTFinish = -1
3283     EndIf
3284     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3285     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3286     Dly 0.1
3287     '�˂����ߊ�����M�𑗐M
3288     M_Out(MOUT_ScwT_FinOK%) = MOn%
3289     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3290     '�˂����ߊJ�n���M�I��
3291     M_Out(MOUT_ScwT_FinOK%) = MOff%
3292     'Wait M_In(MTEST_KEY%) = MOn%
3293     '
3294 *ScewTFinish_ErrEnd
3295 FEnd
3296 '
3297 '
3298 '-----------------------------------------------
3299 '
3300 '����xx��~��M
3301 '
3302 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3303 'fScewTCaseStop = 0�@�F����I��
3304 '          �@   �@-1 �F�ُ�I��
3305 '-----------------------------------------------
3306 Function M% fScewTCaseStop(ByVal MCase%())
3307 *ReCheckScewTCaseStop
3308     fScewTCaseStop = 0
3309     '����xx��~����M
3310     Wait M_In(MCase%(1)) = MOn%
3311     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3312     If MRtn = 0 Then
3313         fScewTCaseStop = -1
3314     EndIf
3315     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3316     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3317     Dly 0.1
3318     '����xx��~��M�𑗐M
3319     M_Out(MCase%(2)) = MOn%
3320     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3321     '�˂����ߊJ�n���M�I��
3322     M_Out(MCase%(2)) = MOff%
3323 *ScewTCaseStop_ErrEnd
3324     '
3325 FEnd
3326 ''��fScrewTighenRoboCheck
3327 '<summary>
3328 '�˂����{�Ď�
3329 '</summary>
3330 '<param name = "MStopNum%"> ��~�ԍ�</param>
3331 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3332 '<make>
3333 '2021/12/2 �����V��
3334 '</make>
3335 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3336     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
3337     fScrewTighenRoboCheck = 1
3338     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3339     MCheck% = 0
3340     While MScrewTighenRoboFlg% = 1
3341         MCheck% = M_In16(11904)
3342         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
3343             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
3344             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
3345         EndIf
3346         If MCheck% <> 0 Then
3347             fScrewTighenRoboError(MCheck%)
3348             Select M_20#
3349                 Case MAbout%            '��~�������ꂽ�ꍇ
3350                     M_Out(12869) = 1 Dly 1.0
3351                     MScrewTighenRoboFlg% = 0
3352                     fScrewTighenRoboCheck = 0   '�ُ�I��
3353                     Break
3354                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3355                     M_Out(12873) = 1 Dly 1.0
3356                     MScrewTighenRoboFlg% = 0
3357                     fScrewTighenRoboCheck = 0   '�ُ�I��
3358                     Break
3359                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3360                     M_20# = MClear%         'M_20#������
3361                     M_Out(12871) = 1 Dly 1.0
3362                     Break
3363                 Case MNext%                 '���ւ������ꂽ�ꍇ
3364                     M_20# = MClear%         'M_20#������
3365                     M_Out(12874) = 1 Dly 1.0
3366                     Break
3367             End Select
3368             Dly 0.5
3369         EndIf
3370     WEnd
3371 FEnd
3372 '
3373 '��fScrewTighenRoboError
3374 '<summary>
3375 '�˂����{�G���[����
3376 '</summary>
3377 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3378 '<make>
3379 '2021/12/2 �����V��
3380 '</make>
3381 Function fScrewTighenRoboError(ByVal MErrorCode%)
3382     MErrorScreenCode% = 0
3383     MErrorScreenCode% = MErrorCode% + 300
3384     fErrorProcess(11,MErrorScreenCode%,0,0)
3385 FEnd
3386 '
3387 '��fErrorProcess
3388 '<summary>
3389 '�G���[����
3390 '</summary>
3391 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3392 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3393 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3394 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3395 '<make>
3396 '2021/11/5 �����V��
3397 '</make>
3398 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3399     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3400     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3401     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3402     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3403 *RETRY_ERR_PROCESS
3404      M_20# = MClear%     '������
3405 '        '�G���[�����L�q
3406         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3407 '        'GOT KEY���͑҂�
3408         MKeyNumber = fnKEY_WAIT()
3409 '        '
3410         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3411             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3412             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3413             Break
3414          '
3415         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3416             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3417             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3418         '
3419         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3420             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3421             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3422          '
3423         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3424             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3425             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3426             Break
3427         '
3428         EndIf
3429         '
3430         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3431 FEnd
3432 '
3433 '��fnTorqueCheck
3434 ''' <summary>
3435 ''' �g���N�`�F�b�N����p�̃��C��
3436 ''' </summary>
3437 ''' <remarks>
3438 ''' Date   : 2021/12/21 : H.AJI
3439 ''' </remarks>'
3440 Function M% fnTorqueCheck
3441     '�g���N�`�F�b�N�����M  �����n��~
3442     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3443     '
3444     fnTorqueCheck = 0
3445     Ovrd 20
3446     Mov PInitialPosition              '�����ʒu�ړ�
3447     Ovrd 100
3448     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3449     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3450     Dly 0.2
3451     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3452     '
3453     'M6340  �g���N�`�F�b�N��M
3454     'Dly 5.0
3455     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3456     Dly 1.0
3457     M_Out(12340) = 0
3458     '
3459     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3460     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3461    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3462     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3463     '
3464     '
3465     MLoopFlg = 1
3466     While MLoopFlg = 1
3467         '
3468         Mov PInitialPosition              '�����ʒu�ړ�
3469         '
3470         MKeyNumber = fnKEY_WAIT()
3471         Select MKeyNumber
3472             Case Is = 1           '��~
3473                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3474                 Dly 1.0
3475                 M_Out(12343) = 0
3476                 Ovrd 20
3477                 'Mov PTicketRead_1
3478                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3479                 Wait M_In(11859) = 1      '�˂����{����̏I��
3480                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3481                 Ovrd 100
3482                 M_20# = 1
3483                 MLoopFlg = -1
3484                 Break
3485             Case Is = 2           '����
3486                 Break
3487             Case Is = 3           '�p��
3488                 Break
3489             Case Is = 4           '�g���N�`�F�b�N�J�n
3490                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3491                 Dly 1.0
3492                 M_Out(12342) = 0
3493                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3494                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3495                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3496                 EndIf
3497                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3498                 'MRet = fnMoveTorquePosi()
3499                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3500                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3501                 Break
3502             Default
3503                 Break
3504         End Select
3505     WEnd
3506     '
3507     '�g���N�`�F�b�N����~���M
3508     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3509     '
3510     '���{�b�g�̈ʒu�����ɖ߂�
3511     '
3512     '
3513  FEnd
3514  '
3515 '
3516 '
3517 '---------------------------
3518 '
3519 '    ���C����ʂ̕\���A��\���ݒ�
3520 '         �R�����gD1001, D1002, D1003�̐ݒ�
3521 '           MWindReSet = 0     ��ʔ�\��
3522 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3523 '           MWindErrScr = 10    �G���[��� D1001, D1002
3524 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3525 '
3526 '---------------------------
3527 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3528     fnMainScreenOpen = 0
3529     '
3530    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3531         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3532     EndIf
3533     '
3534     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3535         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3536     EndIf
3537     '
3538     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3539         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3540     EndIf
3541     '
3542     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3543     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3544     Dly 0.5
3545     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3546 FEnd
3547 '
3548 '��Main
3549 ''' <summary>
3550 ''' �g���N�`�F�b�N������
3551 ''' </summary>
3552 ''' <remarks>
3553 ''' Date   : 2021/12/21 : H.AJI
3554 ''' </remarks>'
3555 Function M% fnScrewMTorque
3556     fnScrewMTorque = 0
3557     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3558     Wait M_In(11857) = 1                     '��M����
3559     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3560     Dly 2.0
3561 FEnd
3562 '
3563 '
3564 '----------------------------------------------------------------
3565 'fTimeOutJudge
3566 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3567 '����
3568 'Address% = �Ď��A�h���X�ԍ�
3569 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3570 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3571 '�߂�l = 0 �G���[
3572 '         1 ����I��
3573 '         2 ���g���C
3574 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3575 '�쐬��
3576 '2022/9/20 ����
3577 '----------------------------------------------------------------
3578 '
3579 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3580     fTimeOutJudge = 0
3581     MJudge% = 1
3582     MRtn = 0
3583     M_20# = MClear%
3584     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3585 *TimeOutLoop
3586     If MRtn = 1 Then GoTo *TimeOut
3587         fErrorProcess(11,202,203,0)
3588         If M_20# = MNext% Then GoTo *TimeOutLoop
3589         If M_20# = MContinue% Then MJudge% = 2
3590         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3591 *TimeOut
3592     fTimeOutJudge = MJudge%
3593 '
3594 *JUDGE_ERROR_END
3595 FEnd
3596 '��Main
3597 ''' <summary>
3598 ''' �g������p�̃��C��
3599 ''' </summary>
3600 ''' <remarks>
3601 ''' Date   : 2021/07/07 : M.Hayakawa
3602 ''' </remarks>'
3603 Function Main
3604     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3605     '
3606     If M_Svo=0 Then
3607         Servo On
3608     EndIf
3609     Wait M_Svo=1
3610 '�g���X�^�[�g���t�����v���p���XON
3611     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3612 '�p�g���C�g����
3613     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3614     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3615     '
3616     M_20# = 0                                   'KEY���͏�����
3617     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3618     MRet% = 0
3619 '�����ʒu�̊m�F�ƈړ�
3620 '
3621 '
3622 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
3623     PActive = P_Curr                    '���݈ʒu���擾
3624     MRecoveryPass% = 0
3625     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3626         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3627             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3628                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3629             EndIf
3630         EndIf
3631     EndIf
3632     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3633         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3634             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3635                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3636             EndIf
3637         EndIf
3638     EndIf
3639     If MRecoveryPass% = 0 Then
3640        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3641     EndIf
3642 '
3643 '
3644 '    MRet% = fnRoboPosChk()
3645 '    If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ
3646 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3647 '        MKeyNumber% = fnKEY_WAIT()
3648 '        Select MKeyNumber%
3649 '            Case Is = MAbout%       '��~
3650 ''                M_20# = MAbout%
3651 '                MLoopFlg% = -1
3652 '                Break
3653 '            Case Is = MNext%        '����
3654 '                'MLoopFlg = -1
3655 '                Break
3656 '            Case Is = MContinue%    '�p��
3657 '                M_20# = MContinue%
3658 '                MLoopFlg% = -1
3659 '                Break
3660 '            Default
3661 '                Break
3662 '        End Select
3663 '    EndIf
3664     '
3665     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3666         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3667 '�g���N�`�F�b�N
3668         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3669             MRet% = fnTorqueCheck()
3670             Break
3671         Else
3672 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3673 '                MRtn = InspInit()               '�摜��������������
3674 '            EndIf
3675             '
3676            M_20# = MClear%                    '������
3677 '�g���J�n
3678             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3679                 fnAssyStart()
3680             Else
3681                 M_20# = MPass%
3682             EndIf
3683 '�g���I�����t����
3684             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3685             Wait M_In(11572) = 1            '���t�擾����
3686             Dly 0.1
3687             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3688 '���t�^�[���j�b�g�ւ�OUT
3689             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3690             fnAutoScreenComment(89)         'AUTO��� �g����������
3691             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3692 'OK/NG�t���O�o��
3693             If M_20# <= 0 Then
3694                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3695             ElseIf M_20# = MPass% Then
3696                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3697             EndIf
3698 'PIAS�ɑg������������
3699             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3700                 If M_20# = MPass% Then
3701                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3702                 Else
3703                     'KEY���͂�NG�̏ꍇ
3704                     If M_20# = MNgProcess% Then
3705                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3706                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3707                         MRet% = fnPiasWrite(MNG%)
3708                        nAssyNgQty = nAssyNgQty + 1
3709                     EndIf
3710                     '
3711                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3712                     If M_20# = MAssyOK% Then
3713                             '-----------------------
3714                             'D732 -> D2600 �R�s�[�v��
3715                             M_Out(12566) = 1
3716 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3717                             M_Out(12566) = 0
3718                             '
3719                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3720                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3721                             '��ԍ��ƍ�(PP�͖��g�p�j
3722 '                            MRet% = fnPCBNumberCheck()
3723                         Else
3724                             MRet% = 1
3725                         EndIf
3726                         '
3727                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3728                             If M_20# <> MAbout% Then
3729                                 '�H������OK��������
3730                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3731                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3732                                 MRet% = fnPiasWrite(MOK%)
3733                                 nAssyOkQty = 0
3734                                 nAssyOkQty = nAssyOkQty + 1
3735                             Else
3736                                 nAssyOkQty = nAssyOkQty + 1
3737                             EndIf
3738                         EndIf
3739                     EndIf
3740 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3741 '                    MRet% = fnPiasWrite(MOK%)
3742                 EndIf
3743             Else
3744                 nAssyOkQty = nAssyOkQty + 1
3745             EndIf
3746             '
3747             '�g���I�����t��������
3748             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3749             '�������A�g��OK���A�g��NG��������
3750 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3751             '
3752 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3753 '                '�摜�����I������
3754 '                MRtn = InspQuit()
3755 '            EndIf
3756         EndIf
3757         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3758     EndIf
3759 '�p�g���C�g����
3760     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3761     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3762 'GOT�\��
3763     fnAutoScreenComment(93)  'AUTO��� �H������
3764 FEnd
3765 End
3766 '
3767 '���܂��Ȃ��R�����g
3768 '��΍폜�����
3769 '
3770 '
3771 '
3772 '
3773 '
3774 '
3775 '
3776 '
JActive=(110.770,45.120,39.400,-0.010,95.510,-73.230)
Jmove=(110.770,-46.870,111.640,0.000,80.580,-73.230,0.000,0.000)
JTaihi=(0.000,-46.870,111.640,0.000,80.580,0.000,0.000,0.000)
PActive=(602.000,-150.000,550.000,-180.000,0.000,90.000)(7,0)
PInitialPosition=(340.000,0.000,580.000,-180.000,0.000,180.000)(7,0)
PMechaGet=(-418.660,-2.920,305.030,180.000,0.000,-179.990)(7,1048577)
PMechaGet_1=(-418.660,-2.920,410.000,180.000,0.000,-179.990)(7,1048577)
PMechaGet_2=(-189.840,-0.010,629.060,-180.000,0.000,-179.990)(7,1)
PMechaGet_3=(0.010,189.840,629.070,-180.000,0.000,90.000)(7,0)
PMechaGet_4=(327.500,0.020,596.240,-179.990,0.000,103.500)(7,0)
PMechaGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet1=(167.100,-331.100,318.570,-87.200,88.180,-177.230)(6,0)
PMechaSet1_1=(167.100,-331.100,340.000,-87.200,88.180,-177.230)(6,0)
PMechaSet2=(169.450,-331.620,319.060,-89.340,88.050,-179.870)(6,0)
PMechaSet2_1=(169.450,-331.620,340.000,-89.340,88.050,-179.870)(6,0)
PMechaSet_2=(162.580,-305.370,557.380,179.470,90.000,89.470)(6,0)
PMechaSet_3=(114.450,-288.220,565.580,180.000,0.000,112.110)(7,0)
PMechaSet_4=(310.110,-0.040,565.560,180.000,0.000,-179.550)(7,0)
PMechaSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
Pmove=(-214.290,565.140,640.000,179.970,-0.020,4.000)(7,0)
PPlateBackCheck=(-90.210,513.030,577.720,-180.000,0.000,-90.000)(7,0)
PPlateBackCheck_2=(66.390,429.860,577.750,180.000,0.000,-90.000)(7,0)
PPlateBackCheck_3=(-18.780,286.220,630.880,180.000,0.000,-90.000)(7,0)
PPlateBackCheck_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet=(478.130,103.510,401.670,179.650,0.130,-179.140)(7,0)
PPlateBackGet_1=(478.130,103.510,430.000,179.650,0.130,-179.140)(7,0)
PPlateBackGet_2=(478.130,103.510,560.000,179.650,0.130,-179.140)(7,0)
PPlateBackGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackPush=(-20.680,418.340,540.820,-180.000,0.000,90.510)(7,1048576)
PPlateBackPush_1=(-20.680,400.000,540.820,-180.000,0.000,90.510)(7,1048576)
PPlateBackPush_2=(-20.680,380.000,564.000,180.000,0.000,90.000)(7,1048576)
PPlateBackSet=(-21.300,459.990,540.880,179.430,-11.000,90.800)(7,1048576)
PPlateBackSet_00=(-20.610,498.130,545.380,179.770,0.000,90.430)(7,1048576)
PPlateBackSet_1=(-21.300,452.590,539.570,179.400,-13.000,90.810)(7,1048576)
PPlateBackSet_10=(-21.270,354.310,478.810,179.080,-44.980,90.810)(7,1048576)
PPlateBackSet_11=(-21.240,351.470,478.810,179.080,-44.980,90.910)(7,1048576)
PPlateBackSet_12=(-20.870,345.000,495.000,179.080,-44.990,90.430)(7,1048576)
PPlateBackSet_13=(-17.880,286.220,630.900,-179.820,-0.290,90.490)(7,1048576)
PPlateBackSet_2=(-21.300,439.300,535.460,179.350,-17.000,90.820)(7,1048576)
PPlateBackSet_3=(-21.300,425.400,530.660,179.290,-21.000,90.840)(7,1048576)
PPlateBackSet_4=(-21.300,413.500,524.450,179.220,-25.000,90.870)(7,1048576)
PPlateBackSet_5=(-21.300,399.710,518.440,179.150,-29.000,90.900)(7,1048576)
PPlateBackSet_6=(-21.300,388.510,509.330,179.080,-33.000,90.940)(7,1048576)
PPlateBackSet_7=(-21.300,377.510,502.430,179.000,-37.000,90.980)(7,1048576)
PPlateBackSet_8=(-21.300,366.410,492.730,178.900,-41.000,91.040)(7,1048576)
PPlateBackSet_9=(-21.300,356.070,482.090,179.080,-44.980,91.110)(7,1048576)
PProductOnPltGet=(479.400,-99.730,372.850,179.990,-0.350,-179.430)(7,0)
PProductOnPltGet_1=(479.400,-99.730,410.000,179.990,-0.350,-179.430)(7,0)
PProductOnPltGet_2=(479.400,-99.730,510.000,179.990,-0.350,-179.430)(7,0)
PProductOnPltGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet=(478.900,-99.730,372.850,179.990,-0.350,-179.430)(7,0)
PProductOnPltSet_1=(478.900,-99.730,410.000,179.990,-0.350,-179.430)(7,0)
PProductOnPltSet_2=(478.900,-99.730,510.000,179.990,-0.350,-179.430)(7,0)
PProductOnPltSet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet=(-19.850,405.990,321.180,-111.550,88.830,-21.440)(6,0)
PProductOnRoboGet_1=(-19.850,405.990,425.200,-112.910,88.920,-23.000)(6,0)
PProductOnRoboGet_2=(-19.850,387.420,425.200,-112.910,88.920,-22.800)(6,0)
PProductOnRoboGet_3=(-17.160,300.000,550.000,-66.190,88.980,23.820)(6,0)
PProductOnRoboGet_4=(-18.160,300.000,550.000,175.040,89.990,-94.950)(6,0)
PProductOnRoboGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet=(-19.850,405.990,321.180,-111.550,88.830,-21.440)(6,0)
PProductOnRoboSet_1=(-19.850,405.990,425.200,-112.910,88.920,-22.800)(6,0)
PProductOnRoboSet_2=(-19.850,387.420,425.200,-112.910,88.920,-23.000)(6,0)
PProductOnRoboSet_3=(-17.160,300.000,550.000,-66.190,88.980,23.820)(6,0)
PProductOnRoboSet_4=(-18.860,404.690,360.000,-102.740,89.080,-12.360)(6,0)
PProductOnRoboSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPushTilt=(-214.290,565.140,463.690,179.970,-0.020,4.000)(7,0)
PPushTilt_1=(-214.290,565.140,479.620,179.970,-0.020,4.000)(7,0)
PPushTilt_2=(-214.290,565.140,620.000,179.970,-0.020,4.000)(7,0)
PPushTilt_3=(0.020,340.000,610.000,-180.000,-0.010,-91.910)(7,0)
PTemp=(602.000,-150.000,550.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PTicketRead=(602.000,-150.000,500.000,180.000,0.000,90.000)(7,0)
PTicketRead_1=(602.000,-150.000,550.000,180.000,0.000,90.000)(7,0)
PTicketRead_2=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(1)=(602.000,-150.000,500.000,180.000,0.000,90.000,0.000,0.000)(7,0)
PInspPosition(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(11)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(12)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(13)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(14)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(15)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(16)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(17)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(18)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(19)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(20)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(21)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(22)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(23)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(24)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(25)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(26)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(27)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(28)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(29)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(30)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
