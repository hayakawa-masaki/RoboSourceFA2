1 ' ===================================
2 '
3 '  21053001 STEP5 Assy5�v���O����
4 '
5 ' �쐬�ҁFM.Hayakawa
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
214 Def Inte MScrewNo
215 Def Inte MReTry
216 '===== <IO�ϐ���`> =====
217 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
218 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
219 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
220 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
221 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
222 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
223 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
224 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
225 '
226 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
227 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
228 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
229 '
230 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
231 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
232 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
233 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
234 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
235 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
236 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
237 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
238 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
239 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
240 '
241 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
242 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
243 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
244 '
245 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
246 '
247 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
248 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
249 '
250 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
251 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
252 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
253 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
254 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
255 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
256 '
257 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
258 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
259 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
260 '
261 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
262 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
263 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
264 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
265 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
266 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
267 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
268 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
269 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
270 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
271 '
272 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
273 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
274 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
275 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
276 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
277 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
278 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
279 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
280 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
281 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
282 '
283 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
284 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
285 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
286 '
287 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
288 '
289 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
290 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
291 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
292 '
293 '����
294 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
295 Def Inte MOn                            '�o��=1
296 Def Inte MOff                           '�o��=0
297 '
298 '�˂����ߑ��u_�o�̓A�h���X
299 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
300 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
301 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
302 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
303 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
304 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
305 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
307 '�˂����ߑ��u_���̓A�h���X
308 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
309 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
310 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
311 Def Inte MIN_ScwT_Case1                 '����1��~����M
312 Def Inte MIN_ScwT_Case2                 '����2��~����M
313 Def Inte MIN_ScwT_Case3                 '����3��~����M
314 Def Inte MIN_ScwT_Case4                 '����4��~����M
315 Def Inte MIN_ScwT_Case5                 '����5��~����M
316 '
317 Dim MScwT_Case1%(2)               '����1��~�ϐ�
318 Dim MScwT_Case2%(2)               '����2��~�ϐ�
319 Dim MScwT_Case3%(2)               '����3��~�ϐ�
320 Dim MScwT_Case4%(2)               '����4��~�ϐ�
321 Dim MScwT_Case5%(2)               '����5��~�ϐ�
322 '
323 '����
324 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
325 MOn% = 1                                 '�o�� = 1
326 MOff% = 0                                '�o�� = 0
327 '
328 '�˂����ߋ@_�A�h���X�ݒ�
329 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
330 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
331 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
332 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
333 MOUT_ScwT_Case1OK% = 12874              '����1��~��M�𑗐M
334 MOUT_ScwT_Case2OK% = 12875              '����2��~��M�𑗐M
335 MOUT_ScwT_Case3OK% = 12876              '����3��~��M�𑗐M
336 MOUT_ScwT_Case4OK% = 12877              '����4��~��M�𑗐M
337 MOUT_ScwT_Case5OK% = 12878              '����5��~��M�𑗐M
338 '
339 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
340 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
341 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
342 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
343 MIN_ScwT_Case1% = 11882                 '����1��~�ҋ@����M
344 MIN_ScwT_Case2% = 11883                 '����2��~�ҋ@����M
345 MIN_ScwT_Case3% = 11884                 '����3��~�ҋ@����M
346 MIN_ScwT_Case4% = 11885                 '����4��~�ҋ@����M
347 MIN_ScwT_Case5% = 11886                 '����5��~�ҋ@����M
348 '
349 MScwT_Case1%(1) = MIN_ScwT_Case1%
350 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
351 MScwT_Case2%(1) = MIN_ScwT_Case2%
352 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
353 MScwT_Case3%(1) = MIN_ScwT_Case3%
354 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
355 MScwT_Case4%(1) = MIN_ScwT_Case4%
356 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
357 MScwT_Case5%(1) = MIN_ScwT_Case5%
358 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
359 '
360 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
361 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
362 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
363 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
364 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
365 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
366 Def Inte MRecoveryPass      '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
367 Def Inte MJ6          'J6���̒l���r����ׂ̕ϐ�
368 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
369 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
370 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
371 '
372 '
373 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
374 Function M% fnAssyStart
375     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
376 '   BaseUnit6�ʐM�m�F CD�ɔ����폜 2022/07/27 M.H
377 '
378 '
379 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
380     M_20# = MClear%                       '������
381 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
382 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
383 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
384 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
385 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
386 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
387 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
388 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
389 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
390 '    EndIf
391 '    '
392 '    '���W�ړ�
393 '    '
394 '    '����xx��~
395 '    fScewTCaseStop(MScwT_Case5%)
396 '    '
397 '    '�x�[�X���j�b�gKEY
398 '    Wait M_In(MTEST_KEY%) = MOn%
399 '    '
400 '    '�ĊJ�n
401 '    fScewTReStart()
402 '    '
403 '    '���W�ړ�
404 '    '
405 '    '�˂����ߊ���
406 '    Mret% = fScewTFinish()
407 ' �l�W���߃e�X�g�I��
408 ' PIAS�e�X�g -----------
409 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
410 '    MRet% = fnPiasWrite(MNG%)
411  '   MRet% = fnPCBNumberCheck()
412 ' PIAS�e�X�g�I�� -------
413 '
414     '�g���J�n(9/6�ǉ�(����))
415     '�v���O�������_
416     Ovrd 100
417     ' �n���h��ԏ�����(10/29�ǉ�M.H)(2/11�C��(����))
418     Cmp Off                     '�R���v���C�A���X���[�h�I��
419     ColChk On                   '�Փˌ��mON
420     If M_In(11266) Then
421         M_Out(12256) = 0
422         M_Out(12257) = 1
423     EndIf
424     If M_In(11269) Then
425         M_Out(12258) = 0
426         M_Out(12259) = 1
427     EndIf
428     If M_In(11271) Then
429         M_Out(12260) = 0
430         M_Out(12261) = 1
431     EndIf
432     *WAIT_HAND_INI
433     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
434     *CompHandIni
435     M_Out(12257) = 0
436     M_Out(12259) = 0
437     M_Out(12261) = 0
438 '
439 '
440 'Dly 5                           '�f�o�b�O�p(22/09/30����)
441     ' �˂����ߋ@�e�X�g�p ----------
442     Mret% = fScrewTcomChk()
443     If Mret% = -1 Then GoTo *ASSY_ERROR_END
444     '�`�P�b�gID��ǂ�
445     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
446     PTemp = P_Curr
447     MRtn = 0
448 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
449 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
450 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
451 '                MRtn = 1
452 '            EndIf
453 '        EndIf
454 '    EndIf
455 '    If MRtn = 1 Then
456 '        Mov PTicketRead
457 '    Else
458 '        Cnt 1 , 10 , 10
459 '        Mov PInitialPosition
460 '        Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
461 '        Cnt 0
462 '        Mvs PTicketRead             'ID�ǂ݈ʒu
463 '    EndIf
464 '
465 ' 2022/04/12 ���S�����֏����ύX �n��
466 ' PInitialPosition �ݐ� MStandby=2
467 ' PTicketRead_1 �ݐ� MStandby=1
468 '
469     MStandby = 0    '�ҋ@�ʒu�t���O��������
470     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
471         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
472             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
473                 MStandby = 2
474             EndIf
475         EndIf
476     EndIf
477     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
478         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
479             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
480                 MStandby = 1
481             EndIf
482         EndIf
483     EndIf
484     If MStandby = 2 Then
485         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
486         Cnt 0
487     EndIf
488     If MStandby <> 0 Then GoTo *PositionOK
489     fErrorProcess(11,230,281,0)            '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
490     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
491     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
492     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
493     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
494     *PositionOK
495 '
496     Mvs PTicketRead             'ID�ǂ݈ʒu
497 ' CD�ɔ����폜 2022/07/27 M.H
498 '    M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
499 '    M_Out(12258) = 1            'DVD���J�`���b�N��ON
500 '
501     '
502     MRtn = 1        'MRtn������
503 *RE_TICKET_READ
504 '    MRtn = fnPiasCheck()               'ID�ǂݎ��
505 'PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
506 'MInspGroup%(1) = 1              '����G�ԍ�
507 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
508 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
509     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
510     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
511     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
512 EndIf
513 If MRtn = 1 Then GoTo *CompRead
514 '
515     '�G���[�����i�ʒu���߂�����
516 *RE_ERR_REL_1
517 If M_20# = MContinue% Then M_20# = MRtn
518 M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
519 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
520 '
521 If MRtn = 1 Then GoTo *CompErrorRelease
522 MRtn = M_20#        'M_20#�ꎞ���
523 M_20# = MClear%
524 fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
525 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
526 If M_20# = MNext% Then M_20# = MRtn
527 If M_20# = MNgProcess% Then M_20# = MAbout%
528 *CompErrorRelease
529 '
530 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
531 If M_20# = MNext% Then M_20# = MPass%
532 Mvs PTicketRead_1                         '22/04/07 �ǉ� �n��
533 GoTo *ASSY_ERROR_END
534 *CompRead
535     fScrewTStart()           '�����ʒu�ύX2/27����)
536 '
537     '�p���b�g���琻�i�����
538     '
539     *RE_POSITIONING
540     '
541     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
542 '    Wait M_In(11273) = 1     '�{�̈ʒu���ߏo�[���o
543     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '�{�̈ʒu���ߏo�[���o
544     If MRtn = 1 Then GoTo *CompPositioning
545     fErrorProcess(11,231,282,0)
546     If M_20# = MNext% Then M_20# = MClear%
547     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549     If M_20# = MContinue% Then GoTo *RE_POSITIONING
550     *CompPositioning
551 '
552     Mov PProductOnPltGet_2      '�{�̎󂯎������_
553 '
554 ''    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu�ύX3/14����)
555 '    MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
556 '    *RE_ERR_REL_2
557 '    If M_20# = MContinue% Then M_20# = MRtn2
558 '    If MRtn = 0 Then
559 '        MRtn2 = 1       'MRtn2������
560 '        M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
561 '        Mov PInitialPosition  '"�C�j�V�����ɖ߂铮��"
562 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
563 '        If MRtn2 = 0 Then
564 '            MRtn2 = M_20#                   '�ʒu���ߖߒ[�G���[�Ȃ�M_20#�������ꎞ���
565 '            M_20# = MClear%                 'M_20#������
566 '            fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
567 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#�ɔ����l����
568 '                '�ʒu���߃G���[���������čH���𔲂���ꍇ��~�������s��
569 '            If M_20# = MNgProcess% Then M_20# = MAbout%
570 '            Break
571 '        EndIf
572 '        Break
573 '            EndIf
574 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
575 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
576 '
577 '    Mov PProductOnPltGet_1      '�{�̎󂯎����
578     '
579     *RE_PLT_GET_1
580     '
581     M_Out(12256) = 0            '�{�̃`���b�N��OFF
582     M_Out(12257) = 1            '�{�̃`���b�N�JON
583     '
584 '    Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
585     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
586     If MRtn = 1 Then GoTo *CompPltGet1
587     fErrorProcess(11,244,284,0)
588     If M_20# = MNext% Then M_20# = MClear%
589     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
590     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
591     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
592     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
593     *CompPltGet1
594     '
595     Mov PProductOnPltGet_1      '�{�̎󂯎����
596     '
597     Ovrd 25
598 '    Fine 0.05 , P
599     Mvs PProductOnPltGet        '�{�̎󂯎��ʒu
600     Dly 0.1
601     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
602     M_Out(12256) = 1            '�{�̃`���b�N��ON
603 '    Fine 0 , P
604     '
605     M_Out(12263) = 1 Dly 0.5                    '�{�̈ʒu���ߖߒ[ON
606 '    Wait M_In(11274) = 1     '�{�̈ʒu���ߖߒ[���o
607     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '�{�̈ʒu���ߖߒ[���o
608     If MRtn = 1 Then GoTo *CompPltGet2
609     M_Out(12256) = 0                            '�{�̃`���b�N��OFF
610     M_Out(12257) = 1                            '�{�̃`���b�N�JON
611     Dly 2.0
612     Mvs PProductOnPltGet_1
613     Mov PProductOnPltGet_2
614     fErrorProcess(11,234,284,0)
615     If M_20# = MNext% Then M_20# = MClear%
616     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
617     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
618     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
619     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
620     Mov PProductOnPltGet_1
621     Mvs PProductOnPltGet
622     M_Out(12257) = 0                            '�{�̃`���b�N�JOFF
623     M_Out(12256) = 1                            '�{�̃`���b�N��ON
624     Dly 2.0
625     *CompPltGet2
626     '
627 '    Wait M_In(11264) = 1        '�{�̌��o
628     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '�{�̌��o
629     If MRtn = 1 Then GoTo *CompPltGet3
630     M_Out(12256) = 0            '�{�̃`���b�N��OFF
631     M_Out(12257) = 1            '�{�̃`���b�N�JON
632     Dly 2.0
633     Mvs PProductOnPltGet_1
634     Mov PProductOnPltGet_2
635     fErrorProcess(11,252,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
641     Mov PProductOnPltGet_1
642     Mvs PProductOnPltGet
643     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
644     M_Out(12256) = 1            '�{�̃`���b�N��ON
645     Dly 2.0
646     *CompPltGet3
647     '
648 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
649     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
650     If MRtn = 1 Then GoTo *CompPltGet4
651     M_Out(12256) = 0            '�{�̃`���b�N��OFF
652     M_Out(12257) = 1            '�{�̃`���b�N�JON
653     Dly 2.0
654     Mvs PProductOnPltGet_1
655     Mov PProductOnPltGet_2
656     Dly 0.1
657     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
658     M_Out(12256) = 1            '�{�̃`���b�N��ON
659     Dly 3.0
660     fErrorProcess(11,245,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
663     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
664     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
665     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
666     M_Out(12256) = 0            '�{�̃`���b�N��OFF
667     M_Out(12257) = 1            '�{�̃`���b�N�JON
668     Dly 2.0
669     Mov PProductOnPltGet_1
670     Mvs PProductOnPltGet
671     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
672     M_Out(12256) = 1            '�{�̃`���b�N��ON
673     Dly 2.0
674     *CompPltGet4
675     '
676     Dly 0.1                     '�O�̂��߃f�B���C
677     Cnt 1 , 100 , 100
678     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
679     Mvs PProductOnPltGet_1      '�{�̎󂯎����
680     MRtn = FnCtlValue2(99)       '�Ǐ��J�n�M��OFF  2022/04/28 �n��
681     Ovrd 100
682     Accel 50 , 50
683     Mov PProductOnPltGet_2      '�{�̎󂯎������_
684     '
685     '���i���˂����{2�ɒu��
686     Mov PProductOnRoboSet_3     '�o�H
687     Accel 100 , 100
688     Cnt 0
689 '    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu,�����ύX3/1����)
690     MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
691     *RE_ERR_REL_2
692     If MRtn = 0 Then
693         Cnt 0
694         Mov PProductOnPltSet_2
695         Mov PProductOnPltSet_1
696         Mvs PProductOnPltSet
697         M_Out(12256) = 0        '�{�̃`���b�N��OFF
698         M_Out(12257) = 1        '�{�̃`���b�N�JON
699         Dly 2.0
700         Mvs PProductOnPltSet_1
701         Mvs PProductOnPltSet_2
702         Mov PInitialPosition
703     EndIf
704     If MRtn = 0 Then GoTo *ASSY_ERROR_END
705     '
706     *RE_ROBO_SET_1
707     '
708     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
709     M_Out(12258) = 1            'DVD���J�`���b�N��ON
710 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
711     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
712     If MRtn = 1 Then GoTo *CompRoboSet1
713     fErrorProcess(11,269,284,0)
714     If M_20# = MNext% Then M_20# = MClear%
715     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
716     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
717     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
718     *CompRoboSet1
719 '
720     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
721 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
722     Ovrd 25
723     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
724     Mvs PProductOnRoboSet       '�˂����{���i�u���ʒu
725     M_Out(12866) = 1 Dly 0.3    '�˂����{2����ĊJ(��~1�`��~2)
726 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
727     MScrewRoboNgFlg% = 0
728     MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
729     If MRtn = 0 Then
730         MScrewRoboNgFlg% = 1
731     EndIf
732 '
733     *RE_ROBO_SET_2
734 '
735     M_Out(12256) = 0            '�{�̃`���b�N��OFF
736     M_Out(12257) = 1            '�{�̃`���b�N�JON
737 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
738     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
739     If MRtn = 1 Then GoTo *CompRoboSet2
740     fErrorProcess(11,244,284,0)
741     If M_20# = MNext% Then M_20# = MClear%
742     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
743     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
744     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
745     *CompRoboSet2
746     '
747     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
748     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
749 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
750     Ovrd 100
751     Cnt 1 , 10 , 10
752     Mov PProductOnRoboSet_3     '�o�H
753     '
754     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
755     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
756 '
757 '
758 '
759     '
760     '�`���g�X���C�_�[������
761     Cnt 1 , 10
762     Mov PPushTilt_3             '�`���g�X���C�_�[���������ς����_
763     Cnt 0
764     Mov PPushTilt_2             '�`���g�X���C�_�[�������_
765     Ovrd 30
766     Mvs PPushTilt_1             '�`���g�X���C�_�[�������
767     Spd 1000
768     Ovrd 5
769     Mvs PPushTilt               '�`���g�X���C�_�[����
770     Spd M_NSpd
771     Ovrd 50
772     Cnt 1 , 1 , 1
773     Mvs PPushTilt_1             '�`���g�X���C�_�[�������
774     Cnt 1 , 10 , 10
775     Ovrd 100
776     Mov PPushTilt_2             '�`���g�X���C�_�[�������_
777     Cnt 1 , 100 , 100
778     '
779     '�w�ʔ����(�R���v���C�A���X���[�h����11/8����)
780     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
781 '    Cnt 1 , 10'�b��
782     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~2�`��~3)
783 '    MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
784 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
785 '    Mov PPlateBackGet_1         '�w�ʔ󂯎����'�b��
786     Cnt 0
787     Mov PPlateBackGet_1         '�w�ʔ󂯎����
788     '
789     *RE_PLATE_GET
790     '
791     Fine 0.05 , P               '�t�@�C������ON
792     Ovrd 25
793     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
794 '    Dly 0.2                     '�ꎞ�R�����g�A�E�g
795     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
796     M_Out(12256) = 1            '�{�̃`���b�N��ON
797     '
798 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
799     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
800     If MRtn = 1 Then GoTo *CompPlateGet_1
801     M_Out(12256) = 0            '�{�̃`���b�N��OFF
802     M_Out(12257) = 1            '�{�̃`���b�N�JON
803     Mvs PPlateBackGet_1
804     fErrorProcess(11,245,293,0) '284��293�ɕύX6/2����
805     If M_20# = MNext% Then M_20# = MClear%
806     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
807     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
808     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
809     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
810     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
811     M_Out(12256) = 1            '�{�̃`���b�N��ON
812     *CompPlateGet_1
813     Fine 0 , P                  '�t�@�C������OFF
814     '
815     Ovrd 5
816     Accel 25 , 100
817     Dly 0.7                     '�f�B���C���Ԓ��ߒ�(�c���͊m��)
818 '    CmpG 0.7,0.7,,,,,,       'X,Y���Q�C����0.7�ɕύX
819 '    ColChk Off                  '�Փˌ��mOFF
820 '    Cmp Pos , &B11          'X,Y���R���v���C�A���X���[�h�J�n
821     Mov PPlateBackGet_1         '�w�ʔ󂯎����
822     Cnt 1 , 10 , 10
823 '    Cmp Off                     '�R���v���C�A���X���[�h�I��
824 '    ColChk On                   '�Փˌ��mON
825     Ovrd 50
826     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
827     Ovrd 100
828     Accel 100 , 100
829     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '�w�ʃp�l���`�F�b�N
830     If MRtn = 1 Then GoTo *CompPlateGet_2
831     Cnt 0
832     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/2����
833     If M_20# = MNext% Then M_20# = MClear%
834     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
835     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
836     If M_20# = MContinue% Then
837         Mov PPlateBackGet_1
838         Dly 0.3
839         M_Out(12256) = 0            '�{�̃`���b�N��OFF
840         M_Out(12257) = 1            '�{�̃`���b�N�JON
841         Dly 2.0
842     EndIf
843     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
844     *CompPlateGet_2    '
845     '�w�ʔ�u��
846 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
847     ColChk Off
848     Cnt 1 , 100 , 100           '100mm�ߖT�ǉ�(221221����)
849     Mov PPlateBackSet_13        '�w�ʔu�����
850     Cnt 1 , 10 , 10
851 '
852     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '������x�w�ʃp�l���`�F�b�N
853     If MRtn = 1 Then GoTo *CompPlateGet_3
854     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/2����
855     If M_20# = MNext% Then M_20# = MClear%
856     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
857     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
858     If M_20# = MContinue% Then
859         Mov PPlateBackGet_2
860         Mov PPlateBackGet_1
861         M_Out(12256) = 0            '�{�̃`���b�N��OFF
862         M_Out(12257) = 1            '�{�̃`���b�N�JON
863         Dly 2.0
864     EndIf
865     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
866     *CompPlateGet_3
867 '
868 ' CD�ɔ����폜 2022/07/27 M.H
869 '    ' ���i�����v�����M
870 '    M_Out(12787) = 1
871 '
872     MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
873 '    If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
874     If MRtn = 0 Then GoTo *ASSY_ERROR_END
875 '
876     Mov PPlateBackSet_12        '�ܓ���O���_
877     Cnt 0
878     Ovrd 25
879     Accel 25 , 25
880     Mvs PPlateBackSet_11        '�ܓ��ꍞ�ݑO
881     Mvs PPlateBackSet_10        '�ܓ��ꍞ��1
882     Mvs PPlateBackSet_9         '�ܓ��ꍞ��2
883 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
884 '    Cmp Pos, &B001000
885     Cnt 1                       '�����Ŏ��̈ړ���(221221����)
886     Mov PPlateBackSet_8         '�o�H1
887     Mov PPlateBackSet_7         '�o�H2
888     Mov PPlateBackSet_6         '�o�H3
889     Mov PPlateBackSet_5         '�o�H4
890     Mov PPlateBackSet_4         '�o�H
891     Mov PPlateBackSet_3         '�o�H6
892     Mov PPlateBackSet_2         '�o�H7
893     Mov PPlateBackSet_1         '�o�H8
894     Mov PPlateBackSet           '�w�ʔ����ʒu
895 '    Cmp Off
896     Accel 100 , 100
897     Cnt 0
898     Dly 0.1
899 '
900     *RE_PLATE_SET
901     M_Out(12256) = 0            '�{�̃`���b�N��OFF
902     M_Out(12257) = 1            '�{�̃`���b�N�JON
903     '
904 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
905     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
906     If MRtn = 1 Then GoTo *CompPlateSet
907     fErrorProcess(11,244,284,0)
908     If M_20# = MNext% Then M_20# = MClear%
909     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
910     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
911     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
912     *CompPlateSet
913     '
914 '
915 '-----�b�艟��-------------------------------------(22/12/21����)��������
916 *RE_BUCK_PUSH
917     M_20# = MClear%
918     Mov PPlateBackPush_2
919 '
920     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
921     M_Out(12256) = 1            '�{�̃`���b�N��ON
922 '
923     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
924 '
925     If MRtn = 1 Then GoTo *CompBuckPushSetting  '����Ȃ牟�������
926 '
927     fErrorProcess(11,245,287,0) '�[�Z���T�[NG���G���[�\��
928         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
929         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
930         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NG�������ꂽ��G���[�G���h��
931         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       '���g���C�������ꂽ�������x����
932 '
933 *CompBuckPushSetting
934 '
935     Mvs PPlateBackPush_1
936     Ovrd 10
937     Mvs PPlateBackPush
938 '    Dly 0.1
939 '�w�ʃN�����v��������(12/15)
940     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
941     MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
942         If MRtn = 0 Then
943             Mvs PPlateBackPush_1
944             Mov PPlateBackSet_12
945             Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
946         EndIf
947         If MRtn = 0 Then GoTo *ASSY_ERROR_END
948 '�w�ʃN�����v�����܂�(12/15)
949     Ovrd 50
950     Mvs PPlateBackPush_1
951 *RE_CHUCK_OPEN
952     M_20# = MClear%
953     M_Out(12256) = 0            '�{�̃`���b�N��OFF
954     M_Out(12257) = 1            '�{�̃`���b�N�JON
955     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
956     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
957     fErrorProcess(11,244,284,0)
958         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
959         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
960         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NG�������ꂽ��G���[�G���h��
961         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       '���g���C�������ꂽ�������x����
962 *CompChuckOpenForBackPush
963 '-----�b�艟��-------------------------------------(22/12/21����)�����܂�
964     '
965     ColChk On
966     Mov PPlateBackSet_13        '�w�ʔu�����
967 '    M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
968     Ovrd 100
969     '
970 ''    ' ���i�����v�����M(�����ʒu�ύX2/27����)
971 '    M_Out(12787) = 1
972     '�˂����{���i�N�����v�Œ�҂�
973 '    Wait M_In(11891) = 1        '�˂����{2��~4��M
974 'MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
975 'If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
976 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
977     '�u���ʒu�摜����
978 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
979 '    Mov PPlateBackCheck_2       '�ʉߓ_
980 '    Mvs PPlateBackCheck         '�m�F�ʒu
981     '
982     'PInspPosition(2) = PPlateBackCheck
983     'MInspGroup(2) = 2
984     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
985     'If MRtn <> 1 Then
986     '   '�G���[����
987     'EndIf
988     '
989 ''    ' ���i�����v�����M
990 '    M_Out(12787) = 1    '�����ʒu�ύX
991 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
992     '
993     '�˂����{�������ݑ҂�
994     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)
995     '
996     'DVD���J����� CD�ɔ����폜 2022/07/27 M.H
997 *Loop_CW_CCW_S
998     *RE_MECHA_SET_1
999 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1000 'If MRtn = 0 Then Mov PInitialPosition   '"�C�j�V�����ɖ߂铮��"
1001 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1002 '
1003     *CompRoboGet1
1004     '
1005 '    Ovrd 50
1006     Mov PProductOnRoboGet_3     '�˂����{���i�������_2����3��
1007 '    Ovrd 20
1008     Mvs PProductOnRoboGet_2     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����2��
1009     Ovrd 20
1010     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1011     Ovrd 10
1012     Mvs PProductOnRoboGet       '�˂����{���i���ʒu
1013     Dly 0.1
1014 '
1015     *RE_ROBO_GET_2
1016 '
1017     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1018     M_Out(12256) = 1            '�{�̃`���b�N��ON
1019 '
1020 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
1021     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
1022     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1023     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1024     M_Out(12257) = 1            '�{�̃`���b�N�JON
1025     Dly 2.0
1026     Mvs PProductOnRoboGet_1
1027     Mvs PProductOnRoboGet_2
1028     Mov PProductOnRoboGet_3
1029     Mov PProductOnRoboGet_4
1030     Mov PInitialPosition
1031     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1032     M_Out(12256) = 1            '�{�̃`���b�N��ON
1033     Dly 1.0
1034     fErrorProcess(11,245,284,0)
1035     If M_20# = MNext% Then MRtn = 1
1036     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1037     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1038     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1039     M_Out(12257) = 1            '�{�̃`���b�N�JON
1040     Dly 2.0
1041     Mov PProductOnRoboGet_4
1042     Mov PProductOnRoboGet_3
1043     Mov PProductOnRoboGet_2
1044     Mvs PProductOnRoboGet_1
1045     Mvs PProductOnRoboGet
1046     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1047     *CompRoboGet2
1048     M_20# = MClear%
1049     '
1050     Dly 0.1
1051     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1052     Ovrd 50
1053     Mvs PProductOnRoboGet_2     '�˂����{���i�������_(9/27�b��R�����g�A�E�g)12/15�R�����g����
1054     Mvs PProductOnRoboGet_3     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����3��
1055     Ovrd 100
1056     Mov PProductOnRoboGet_4     '�o�H3����4��
1057     Cnt 1 , 100 , 100
1058 '
1059     M_Out(12868) = 1 Dly 0.3    '�˂����{2���슮���𑗐M
1060     *RE_ROBO_GET_3
1061 ' CD�ɔ����폜 2022/07/27 M.H
1062 '    M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1063 '    M_Out(12259) = 1            'DVD���J�`���b�N�JON
1064 ''    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1065 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1066 '    If MRtn = 1 Then GoTo *CompRoboGet3
1067 '    fErrorProcess(11,270,284,0)
1068 '    If M_20# = MNext% Then M_20# = MClear%
1069 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1070 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1071 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1072     *CompRoboGet3
1073     '
1074     '�p���b�g�֐��i��u��
1075     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1076     Cnt 1 , 10
1077     Mov PProductOnPltSet_1      '�{�̒u���ʒu���
1078     Cnt 0
1079     Ovrd 10
1080     Mvs PProductOnPltSet        '�{�̒u���ʒu
1081     Dly 0.1
1082 '
1083     *RE_PLT_SET
1084 '
1085     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1086     M_Out(12257) = 1            '�{�̃`���b�N�JON
1087 '
1088     Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
1089 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1090     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1091     If MRtn = 1 Then GoTo *CompPltSet
1092     fErrorProcess(11,244,284,0)
1093     If M_20# = MNext% Then M_20# = MClear%
1094     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1095         Mvs PProductOnPltSet_1
1096         Mov PProductOnPltSet_2
1097         Mov PInitialPosition
1098     EndIf
1099     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1100     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1101     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1102     *CompPltSet
1103 '
1104     Mvs PProductOnPltSet_1      '�{�̒u���ʒu���
1105     Ovrd 100
1106     Cnt 1 , 10 , 10
1107     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1108 '    Mov PInitialPosition        '�C�j�V�����|�W�V����
1109     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1110     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
1111     Cnt 0
1112     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1113     '
1114     '�`�P�b�gID��������
1115     M_20# = MAssyOK%
1116     *ASSY_ERROR_END
1117     *AssyEnd
1118     *fnAssyStart_FEndPosi
1119 FEnd
1120 '
1121 '��fnPiasCheck
1122 ''' <summary>
1123 ''' PIAS�`�P�b�g�Ǎ���
1124 ''' </summary>
1125 ''' <returns>   0 : NG
1126 '''             1 : OK(�Ǎ��݊���)
1127 ''' </returns>
1128 ''' <remarks>
1129 ''' Date   : 2021/07/07 : M.Hayakawa
1130 ''' </remarks>'
1131 Function M% fnPiasCheck
1132     fnPiasCheck = 0
1133     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1134     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1135 '
1136 *RETRY_PIAS
1137     M_20# = MClear%
1138     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1139     '
1140     '�yID�`�P�b�g�ǂݍ��݁z
1141     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1142     MInspGroup%(1) = 1              '����G�ԍ�
1143     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1144 '
1145     '�G���[�̏ꍇ
1146     If MRtn <> 1 Then
1147         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1148         If MRtn <> 1 Then
1149             'D720 -> D1300 �R�s�[�v��
1150             M_Out(12565) = 1
1151             Dly 0.5
1152             M_Out(12565) = 0
1153             '�G���[�����L�q
1154             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1155             'GOT KEY���͑҂�
1156             MKeyNumber = fnKEY_WAIT()
1157             '
1158             Select MKeyNumber
1159                 Case MNext%         '���ւ�I�������ꍇ
1160                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1161                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1162                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1163                     Break
1164                 Case MAbout%        '��~��I�������ꍇ
1165                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1166                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1167                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1168                     Break
1169                 Case MNgProcess%    'NG��I�������ꍇ
1170                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1171                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1172                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1173                     Break
1174                 Case MContinue%     '�p����I�������ꍇ
1175                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1176                     M_20# = MContinue%
1177                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1178                     Break
1179             End Select
1180         EndIf
1181     EndIf
1182 '----------D720 -> D1300 �R�s�[�v��----------
1183     M_Out(12565) = 1
1184     Dly 0.5
1185     M_Out(12565) = 0
1186 '----------�ʐM�m�F������----------
1187     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1188     MRtn = 0                ' ������
1189     M_20# = MClear%         ' ������
1190     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1191     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1192     If MRtn <> 1 Then
1193         If M_20# = MContinue% Then
1194             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1195         Else
1196             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1197         EndIf
1198     EndIf
1199 '----------�H�������m�F----------
1200     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1201     MRtn = 0                ' ������
1202     M_20# = MClear%         ' ������
1203     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1204     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1205     If MRtn <> 1 Then
1206         If M_20# = MContinue% Then
1207             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1208         Else
1209             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1210         EndIf
1211     EndIf
1212     '
1213     fnPiasCheck = 1
1214     *fnPiasCheck_End
1215 FEnd
1216 '
1217 '��fnPCComuCheck
1218 ''' <summary>
1219 ''' PC-PLC�ʐM�`�F�b�N
1220 ''' </summary>
1221 ''' <returns>   0 : NG
1222 '''             1 : OK(�Ǎ��݊���)
1223 ''' </returns>
1224 ''' <remarks>
1225 ''' Date   : 2021/07/07 : M.Hayakawa
1226 ''' </remarks>'
1227 Function M% fnPCComuCheck
1228     fnPCComuCheck = 0
1229     MJudge% = 0                                  '������
1230     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1231     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1232     '
1233     For MStaNo = 0 To 5
1234         '
1235         If M_In(MIN_PIAS_ComOK%) = 1 Then
1236             'PC�ʐMOK(M400)
1237             MJudge% = MOK%
1238             MStaNo = 5
1239             Break
1240         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1241             'toRBT_�ʐM�m�Ftime out
1242             MJudge% = MNG%
1243             MCommentD1001 = 15
1244             MCommentD1002 = 21
1245             MStaNo = 5
1246             Break
1247         Else
1248             'toRBT_�ʐM�m�Ftime out
1249             MJudge% = MNG%
1250             MCommentD1001 = 14
1251             MCommentD1002 = 21
1252             Break
1253         EndIf
1254     Next MStaNo
1255     '
1256     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1257     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1258     '
1259     '�G���[���
1260     If MJudge% <> MOK% Then
1261         M_20# = MClear%     '������
1262         '�G���[�����L�q
1263         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1264         'GOT KEY���͑҂�
1265         MKeyNumber = fnKEY_WAIT()
1266         '
1267         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1268             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1269             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1270             Break
1271         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1272             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1273             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1274             Break
1275         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1276             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1277             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1278             Break
1279         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1280             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1281             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1282             Break
1283         EndIf
1284     Else
1285         'OK�̏ꍇ
1286         fnPCComuCheck = 1
1287     EndIf
1288 FEnd
1289 '
1290 '��fnProcessCheck
1291 ''' <summary>
1292 ''' �H�������m�F
1293 ''' </summary>
1294 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1295 '''             -1�F�O�H������NG  -2�F���H����������
1296 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1297 '''             -5�F���������G���[
1298 ''' </returns>
1299 ''' <remarks>
1300 ''' Date   : 2021/07/07 : M.Hayakawa
1301 ''' </remarks>'
1302 Function M% fnProcessCheck
1303     fnProcessCheck = 0
1304     MJudge% = MNG%      '��UNG���������Ƃ���
1305 '----------�H�������m�F----------
1306     MCommentD1001 = 0   '�R�����g������
1307     For MStaNo = 0 To 5
1308         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1309         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1310         '
1311         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1312             MJudge% = MOK%
1313             fnAutoScreenComment(85)     ' AUTO���
1314             MStaNo = 5
1315             Break
1316         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1317             MFlgLoop% = 0
1318             MJudge% = MNG%
1319             MCommentD1001 = 27
1320             MCommentD1002 = 22
1321             fnAutoScreenComment(94)     ' AUTO���
1322             fnProcessCheck = -2         ' NG��-2��Ԃ�
1323             MStaNo = 5
1324             Break
1325         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1326            MJudge% = MNG%
1327             MCommentD1001 = 31
1328             MCommentD1002 = 22
1329             fnAutoScreenComment(83)     ' AUTO���
1330             fnProcessCheck = -3         ' NG��-3��Ԃ�
1331             MStaNo = 5
1332             Break
1333         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1334             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1335             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1336             MJudge% = MNG%
1337             MCommentD1001 = 32
1338             MCommentD1002 = 22
1339             fnAutoScreenComment(84)     ' AUTO���
1340             fnProcessCheck = -1         ' NG��-1��Ԃ�
1341             Dly 1.0
1342             '�H�������m�FOFF
1343             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1344             Dly 1.0
1345            'MStaNo = 5
1346             Break
1347         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1348             MFlgLoop% = 0
1349             MJudge% = MNG%
1350             MCommentD1001 = 29
1351             MCommentD1002 = 22
1352             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1353             fnProcessCheck = -5         ' NG��-5��Ԃ�
1354             MStaNo = 5
1355             Break
1356         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1357             MJudge% = MNG%
1358             If MCommentD1001 = 32 Then
1359                 '�������Ȃ�
1360             Else
1361                 MCommentD1001 = 26
1362             EndIf
1363             MCommentD1002 = 22
1364             fnProcessCheck = -4         ' NG��-4��Ԃ�
1365             MStaNo = 5
1366             Break
1367         Else
1368             MJudge% = MNG%
1369             MCommentD1001 = 28
1370             MCommentD1002 = 22
1371         EndIf
1372     Next MStaNo
1373     '�H�������m�FOFF
1374     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1375     '�ʉߗ���NG �H�������̏ꍇ
1376     If MJudge% = MPass% Then
1377         M_20# = MPass%
1378     EndIf
1379     '
1380     '�G���[���
1381     If MJudge% <> MOK% Then
1382         M_20# = MClear%     '������
1383         '�G���[�����L�q
1384         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1385         'GOT KEY���͑҂�
1386         MKeyNumber = fnKEY_WAIT()
1387         '
1388         Select MKeyNumber
1389             Case MAbout%        '��~��I�������ꍇ
1390                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1391                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1392                 Break
1393             Case MNext%         '���ւ�I�������ꍇ
1394                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1395                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1396                 Break
1397             Case MContinue%     '�p����I�������ꍇ
1398                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1399                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1400                 Break
1401             Case MNgProcess%    'NG��I�������ꍇ
1402                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1403                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1404                 Break
1405         End Select
1406     Else
1407         fnProcessCheck = 1  ' OK��1��Ԃ�
1408     EndIf
1409 FEnd
1410 '
1411 '��fnPiasWrite
1412 ''' <summary>
1413 ''' Pias �g�����ʏ����ݗv��
1414 ''' </summary>
1415 '''<param name="MFlg%">
1416 '''                 MOK%(1) = �H��������OK��������
1417 '''                 MNG%(0) = �H��������NG��������
1418 '''</param>
1419 '''<returns></returns>
1420 ''' <remarks>
1421 ''' Date   : 2021/07/07 : M.Hayakawa
1422 ''' </remarks>'
1423 Function M% fnPiasWrite(ByVal MFlg%)
1424       fnPiasWrite = 0
1425 *RETRY_PIASWRITE
1426     '
1427     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1428    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1429     If MFlg% = MOK% Then
1430         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1431     Else
1432         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1433     EndIf
1434     Dly 0.1                  '�O�̂���
1435     '
1436     'Pias�֏����݊J�n M305 -> ON
1437     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1438     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1439     '
1440     MJudge% = MNG%
1441     '
1442     For MStaNo = 0 To 5
1443         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1444             MJudge% = MOK%
1445             'MRet = fnAutoScreenComment(85)  'AUTO���
1446             MStaNo = 5
1447             Break
1448         '
1449         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1450             MJudge% = MNG%
1451             'MRet = fnAutoScreenComment(85)  'AUTO���
1452            MCommentD1001 = 34
1453            MCommentD1002 = 25
1454             MStaNo = 5
1455             Break
1456         '
1457         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1458             MJudge% = MNG%
1459             'MRet = fnAutoScreenComment(85)  'AUTO���
1460            MCommentD1001 = 35
1461            MCommentD1002 = 25
1462             MStaNo = 5
1463             Break
1464         '
1465         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1466             MJudge% = MNG%
1467             'MRet = fnAutoScreenComment(85)  'AUTO���
1468            MCommentD1001 = 36
1469            MCommentD1002 = 25
1470             MStaNo = 5
1471             Break
1472         '
1473         Else
1474             MJudge% = MNG%
1475            MCommentD1001 = 42
1476            MCommentD1002 = 25
1477         '
1478         EndIf
1479         '
1480     Next MStaNo
1481     '
1482     'Pias�֏����݊J�n M305 -> OfF
1483     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1484     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1485     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1486     '
1487     '
1488     '�ʉߗ���NG �H�������̏ꍇ
1489     If MJudge% = MPass% Then
1490         M_20# = MPass%
1491     EndIf
1492     '
1493    M_20# = MClear%     '������
1494     '
1495     '�G���[���
1496     If MJudge% < MOK% Then
1497     '
1498 '�c���Ă���������ł͎g�p���Ȃ����x��
1499 *RETRY_ERR_WRITE
1500         M_20# = MClear%     '������
1501         '�G���[�����L�q
1502         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1503         'GOT KEY���͑҂�
1504         MKeyNumber = fnKEY_WAIT()
1505         '
1506         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1507             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1508            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1509             Break
1510         '
1511         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1512             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1513             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1514         '
1515         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1516             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1517             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1518         '
1519         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1520             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1521            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1522             Break
1523         '
1524         EndIf
1525         '
1526         If M_20# = MClear% Then *RETRY_ERR_WRITE
1527         '
1528     EndIf
1529     '
1530     If M_20# = MContinue% Then *RETRY_PIASWRITE
1531     '
1532     fnPiasWrite = 1
1533     '
1534 FEnd
1535 '
1536 '��fnPCBNumberCheck
1537 ''' <summary>
1538 ''' Pias ��ԍ��ƍ��v��
1539 ''' </summary>
1540 '''<param name="%"></param>
1541 '''<param name="%"></param>
1542 '''<returns></returns>
1543 ''' <remarks>
1544 ''' Date   : 2021/07/07 : M.Hayakawa
1545 ''' </remarks>'
1546 Function M% fnPCBNumberCheck
1547       fnPCBNumberCheck = 0
1548     '
1549 *RETRY_PCBCHECK
1550     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1551     'Pias�֊�ƍ��J�n M310 -> ON
1552     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1553     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1554     '
1555     MJudge% = MNG%
1556     '
1557     For MStaNo = 0 To 5
1558         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1559             MJudge% = MOK%
1560             fnAutoScreenComment(96)  'AUTO���
1561             MStaNo = 5
1562             Break
1563         '
1564         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1565             MJudge% = MNG%
1566             fnAutoScreenComment(97)  'AUTO���
1567             MCommentD1001 = 37
1568             MCommentD1002 = 25
1569             MStaNo = 5
1570             Break
1571         '
1572         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1573             MJudge% = MNG%
1574             fnAutoScreenComment(98)  'AUTO���
1575             MCommentD1001 = 38
1576             MCommentD1002 = 25
1577             MStaNo = 5
1578             Break
1579         '
1580         ElseIf M_In(11580) = 1 Then                         'time out
1581             MJudge% = MNG%
1582             fnAutoScreenComment(99)  'AUTO���
1583             MCommentD1001 = 39
1584             MCommentD1002 = 25
1585             MStaNo = 5
1586             Break
1587         '
1588         Else
1589             MJudge% = MNG%
1590            MCommentD1001 = 41
1591            MCommentD1002 = 25
1592         '
1593         EndIf
1594         '
1595     Next MStaNo
1596     '
1597     'Pias�֊�ƍ��J�n M310 -> OfF
1598     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1599     '
1600     '
1601     '�ʉߗ���NG �H�������̏ꍇ
1602     If MJudge% = MPass% Then
1603         M_20# = MPass%
1604     EndIf
1605     '
1606    M_20# = MClear%     '������
1607     '
1608     '�G���[���
1609     If MJudge% < MOK% Then
1610     '
1611 '�c���Ă���������ł͎g�p���Ȃ����x��
1612 *RETRY_ERR_PCBNUMBER
1613         M_20# = MClear%     '������
1614         '�G���[�����L�q
1615         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1616         'GOT KEY���͑҂�
1617         MKeyNumber = fnKEY_WAIT()
1618         '
1619         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1620             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1621             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1622             Break
1623         '
1624         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1625             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1626             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1627         '
1628         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1629             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1630             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1631         '
1632         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1633             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1634             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1635             Break
1636         '
1637         EndIf
1638         '
1639         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1640         '
1641     EndIf
1642     '
1643     If M_20# = MContinue% Then *RETRY_PCBCHECK
1644 FEnd
1645 '
1646 '��ScrewTight_S2
1647 ''' <summary>
1648 ''' �˂����߂��s��
1649 ''' </summary>
1650 '''<param name="PScrewPos()">
1651 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1652 '''             PScrewPos(2)    �F�˂����߉��_
1653 '''             PScrewPos(10)   �F�˂����ߏI������
1654 '''</param>
1655 '''<returns>����
1656 '''         0=�ُ�I���A1=����I��
1657 '''</returns>
1658 ''' <remarks>
1659 ''' Date   : 2021/07/07 : M.Hayakawa
1660 ''' </remarks>'
1661 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1662     ScrewTight_S2 = 0
1663     MOKNGFlg = 0
1664     Ovrd 100
1665     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1666     ' �b��
1667     Ovrd 5
1668     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
1669 '    Ovrd MOvrdA
1670     '�b��}�X�N
1671 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1672 '    Dly 0.1
1673 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1674 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
1675 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
1676     ' �b��ړ��̂�
1677     Mvs PScrewPosition(10)
1678 '    '
1679 '    Dly 0.1
1680 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1681 '    Wait M_In(11584)=1          '����/�G���[���o
1682 '    Dly 0.1
1683 '    Spd M_NSpd
1684 '    '
1685 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
1686 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1687 '        Dly 0.1
1688 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1689 '        Dly 0.1
1690 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1691 '        Dly 0.1
1692 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
1693 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1694 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1695 '        MOKNGFlg = -1
1696 '        ScrewTight_S2 = 0
1697 '    Else
1698 '        Wait M_In(X29_Driver)=1 ' ���튮����
1699 '        Dly 0.1
1700 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1701 '        Dly 0.1
1702 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
1703 '        Dly 0.1
1704 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1705 '        Dly 0.1
1706 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1707 '        ScrewTight_S2 = 1
1708 '    EndIf
1709 ' �b��
1710     Ovrd 10
1711     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1712     Ovrd 100
1713 FEnd
1714 '
1715 '��ScrewGet_S3
1716 ''' <summary>
1717 ''' �˂������@����˂��𓾂�
1718 ''' </summary>
1719 '''<param name="%"></param>
1720 '''         PScrewPos(1)    �F�˂�������̂˂����
1721 '''         PScrewPos(2)    �F�˂���������_
1722 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1723 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1724 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1725 '''<returns>����
1726 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
1727 '''</returns>
1728 ''' <remarks>
1729 ''' Date   : 2021/07/07 : M.Hayakawa
1730 ''' </remarks>'
1731 Function M% ScrewGet_S3(ByVal PScrewPosition())
1732     ScrewGet_S3 = 0
1733     MMScrewJudge% = 0
1734     '�˂������평������G���[�`�F�b�N
1735 ' ���b��폜
1736 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
1737 '    Ovrd 100
1738 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
1739 '        Ovrd 30
1740 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
1741 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
1742 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
1743 '        'NG�Ƃ��Ă����̊֐����甲����
1744 '        ScrewGet_S3 = -1
1745 '        MMScrewJudge% = 1
1746 '        MCommentD1001 = 61
1747 '    EndIf
1748 '    If ScrewGet_S3 = 0 Then
1749 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
1750 '        MMScrewJudge% = 0 'MMScrewJudge������������
1751 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1752 '        If MRtn = 0 Then
1753 '            Ovrd 30
1754 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
1755 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
1756 '            MMScrewJudge% = 2
1757 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
1758 '            MCnt% = 2   '2��ݒ�
1759 '            MCommentD1001 = 62
1760 '        EndIf
1761 '        If MMScrewJudge% = 2 Then
1762 '            ScrewGet_S3 = -2
1763 '        EndIf
1764 '    EndIf
1765 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
1766 '    If MMScrewJudge% = 2 Then
1767 '        ScrewGet_S3 = -2
1768 '    EndIf
1769     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
1770     Ovrd 100
1771     Spd M_NSpd
1772     If MMScrewJudge% = 0 Then
1773         ScrewGet_S3 = 0
1774         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1775         MScrewCnt% = 0
1776         MFinCnt% = 2
1777 '        For MCnt% = 0 To MFinCnt%
1778             Mov PScrewPosition(2)        ' �˂������@���_
1779             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1780             Ovrd 80
1781             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1782             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1783             Mvs PScrewPosition(10), 1.2
1784             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
1785             '�r�b�g��]
1786             M_Out(Y60_Driver)=1
1787             Dly 0.2
1788             '
1789             Ovrd 100
1790             JOvrd M_NJovrd
1791             Spd M_NSpd
1792             '�l�W�z���m�F�ʒu�ړ�
1793             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1794             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
1795             '�r�b�g��]��~
1796             'M_Out(Y60_Driver)=0
1797             '
1798             '1�b�ԃl�W�z���m�F
1799 ' �ȉ��b��폜
1800 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1801 '            'MRtn = 0'�����G���[
1802 '            '�z���G���[�̏ꍇ
1803 '            '�l�W���˂����Y�ɖ߂�
1804 '            If MRtn = 0 Then
1805 '                Ovrd 30
1806 '                '�r�b�g��]��~
1807 '                M_Out(Y60_Driver)=0
1808 '                '�l�W�����@���
1809 '                Mvs PScrewPos(1)
1810 '                '�X�ɏ��
1811 '                Mov PScrewPos(1), -75
1812 '                '�l�W�̂Ĉʒu
1813 '                Mov PScrewFeedS021
1814 '                '�z��OFF
1815 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
1816 '                Dly 0.2
1817 '                '�j��ON
1818 '                M_Out(Y6B_VB1)=1 '�^��j��ON
1819 '                '�r�b�g��]
1820 '                M_Out(Y61_Driver)=1
1821 '                Dly 0.5
1822 '                '
1823 '                Ovrd 100
1824 '                JOvrd M_NJovrd
1825 '                Spd M_NSpd
1826 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1827 '                Mov PScrewFeedS021, 10
1828 '                Mov PScrewFeedS021
1829 '                Dly 0.1
1830 '                Mov PScrewFeedS021, 10
1831 '                Mov PScrewFeedS021
1832 '                '
1833 '                '�l�W�����҂�
1834 '                '�r�b�g��]��~
1835 '                M_Out(Y61_Driver)=0
1836 '                Dly 0.1
1837 '                '�j��OFF
1838 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
1839 '                '
1840 '                '
1841 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
1842 '                Mov PScrewPos(1), -75
1843 '                Ovrd 100
1844 '                Spd M_NSpd
1845 '                '�l�W�����@���
1846 '                Mvs PScrewPos(1)
1847 '                '
1848 '                ScrewGet_S3 = -3
1849 '                Break
1850 '                '
1851 '            Else
1852 '                MCnt% = MFinCnt%
1853 '                ScrewGet_S3 = 0
1854 '            EndIf
1855 '        Next  MCnt%
1856         '
1857         Ovrd 100
1858         Spd M_NSpd
1859         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
1860         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1861         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1862         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
1863         '������x�z���m�F
1864 ' �ȉ��b��폜
1865 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1866 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1867 '            MCommentD1001 = 94
1868 '            MCommentD1002 = 95
1869 '            ScrewGet_S3 = -3
1870 '        EndIf
1871 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
1872 '            ScrewGet_S3 = 1
1873 '        EndIf
1874 '        Break
1875     Else
1876         'M�l�W
1877         If MMScrewJudge% = 2 Then
1878             ScrewGet_S3 = -2
1879         EndIf
1880     EndIf
1881 FEnd
1882 '
1883 '��fnKEY_WAIT()
1884 ''' <summary>
1885 ''' GOT����̃L�[���͑҂�
1886 ''' </summary>
1887 '''<returns>1�F��~    2�F����
1888 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1889 '''         5�FNG
1890 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1891 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1892 '''</returns>
1893 ''' <remarks>
1894 ''' Date   : 2021/07/07 : M.Hayakawa
1895 ''' </remarks>'
1896 Function M% fnKEY_WAIT()
1897     fnKEY_WAIT = 0
1898     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1899     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1900     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1901     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1902     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1903     Dly 0.2
1904     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1905     MLocalLoopFlg=1
1906     While MLocalLoopFlg=1
1907         If M_In(11345) = 1 Then         '��~   M5345
1908             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1909             fnKEY_WAIT = 1
1910             MLocalLoopFlg=-1
1911             Break
1912         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1913             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1914             fnKEY_WAIT = 2
1915             MLocalLoopFlg=-1
1916             Break
1917         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1918             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1919             fnKEY_WAIT = 3
1920             MLocalLoopFlg=-1
1921             Break
1922         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1923             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1924             fnKEY_WAIT = 4
1925             MLocalLoopFlg=-1
1926             Break
1927         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1928             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1929             fnKEY_WAIT = 5
1930             MLocalLoopFlg=-1
1931             Break
1932             '
1933         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1934             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1935             fnKEY_WAIT = MRobotInit1%
1936             MLocalLoopFlg=-1
1937             Break
1938             '
1939         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1940             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1941             fnKEY_WAIT = MRobotInit2%
1942             MLocalLoopFlg=-1
1943             Break
1944             '
1945         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1946             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1947             fnKEY_WAIT = MRobotInit3%
1948             MLocalLoopFlg=-1
1949             Break
1950             '
1951         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1952             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1953             fnKEY_WAIT = MRobotInit4%
1954             MLocalLoopFlg=-1
1955             Break
1956             '
1957         Else
1958         EndIf
1959     WEnd
1960     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1961     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
1962 FEnd
1963 '
1964 '�� fnAUTO_CTL
1965 ''' <summary>
1966 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
1967 ''' </summary>
1968 ''' <remarks>
1969 ''' Date   : 2021/07/07 : M.Hayakawa
1970 ''' </remarks>
1971 Function M% fnAUTO_CTL
1972     fnAUTO_CTL = 0
1973     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1974     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
1975     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1976     '
1977     If M_Svo=0 Then             '�T�[�{ON�m�F
1978         Servo On
1979     EndIf
1980     Wait M_Svo=1
1981 FEnd
1982 '
1983 '�� fnWindScreenOpen
1984 ''' <summary>
1985 ''' �E�B���h��ʂ̕\���A��\���ݒ�
1986 ''' </summary>
1987 '''<param name="%"></param>
1988 '''<param name="%"></param>
1989 '''<param name="%"></param>
1990 '''<param name="%"></param>
1991 ''' <remarks>
1992 ''' �R�����gD1001, D1002, D1003�̐ݒ�
1993 ''' MWindReSet = 0     ��ʔ�\��
1994 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
1995 ''' MWindErrScr = 10    �G���[��� D1001, D1002
1996 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
1997 ''' Date   : 2021/07/07 : M.Hayakawa
1998 ''' </remarks>
1999 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2000     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2001         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2002     EndIf
2003     '
2004     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2005         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2006     EndIf
2007     '
2008     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2009        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2010     EndIf
2011     '
2012     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2013     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2014     Dly 0.5
2015     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2016 FEnd
2017 '
2018 '��FnCtlValue2
2019 ''' <summary>
2020 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2021 ''' </summary>
2022 ''' <param name="MCtlNo%"></param>
2023 ''' <remarks>
2024 ''' Date : 2022/04/28 �n��
2025 ''' </remarks>
2026 '''
2027 '''  1�F������       �{�P
2028 '''  2�F�g���n�j��   �{�P
2029 '''  3�F�g���m�f��   �{�P (���g�p)
2030 '''  4�F�z���G���[�� �{�P
2031 ''' 99�F�Ǐ��J�n�M�� OFF
2032 '''
2033 Function M% FnCtlValue2(ByVal MCtlNo%)
2034     FnCtlValue2 = 1
2035     Select MCtlNo%
2036         Case 1        '�������{�P
2037             M_Out(12569) = 0             '�����݊J�n�M��OFF
2038             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2039             MInputQty = M_In16(11600)    '��������M
2040             MInputQty = MInputQty + 1    '�������{�P
2041             M_Out16(12592) = MInputQty   '���������M
2042             M_Out(12569) = 1             '�����݊J�n�M��ON
2043             Break
2044             '
2045         Case 2        '�g���n�j���{�P
2046             M_Out(12569) = 0             '�����݊J�n�M��OFF
2047             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2048             MAssyOkQty = M_In16(11616)   '�g��OK����M
2049             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2050             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2051             M_Out(12569) = 1             '�����݊J�n�M��ON
2052             Break
2053             '
2054         Case 4        '�z���G���[���{�P
2055             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2056             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2057             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2058             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2059             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2060             M_Out(12569) = 1                       '�����݊J�n�M��ON
2061             Break
2062             '
2063         Case 99        '�Ǐ��J�n�M��OFF
2064             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2065             M_Out(12569) = 0        '�����݊J�n�M��OFF
2066             Break
2067             '
2068     End Select
2069     Exit Function
2070 FEnd
2071 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2072 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2073 '-------------------------------------------------------------------------------
2074 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2075 '   ����
2076 '       PInspPos()      �F�����ʒu
2077 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2078 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2079 '       MInspCnt%       �F�����ʒu��
2080 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2081 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2082 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2083 '   �߂�l�F����
2084 '       0=�ُ�I���A1=����I��
2085 '
2086 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2087 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2088 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2089 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2090 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2091 '-------------------------------------------------------------------------------
2092     '----- �����ݒ� -----
2093     Cnt 0                                                           '�ړ�����������(�����l=0)
2094     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2095 '    Cnt 1,0.1,0.1
2096     '�ϐ��錾�E������
2097     Def Inte MNum                                                   '�����ԍ�(������1�`)
2098     MNum% = 1                                                       '�����ԍ������l�ݒ�
2099     Def Inte MEndFlg                                                '�����I���t���O
2100     MEndFlg% = 0
2101     '
2102     '����G�ԍ��ݒ�v���E�������s�v��off
2103     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2104     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2105     '�G���[�ԍ��N���A
2106     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2107     M_Out16(MOUT_InspErrNum) = MInspErrNum
2108     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2109     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2110     '
2111     'Insight Ready check?
2112     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2113         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2114         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2115         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2116         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2117         Exit Function
2118     EndIf
2119     '
2120     '�����ʒu���m�F
2121     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2122         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2123         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2124         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2125         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2126         Exit Function
2127     EndIf
2128     '
2129     '
2130     '
2131     '----- ���C������ -----
2132     '�ݒ肳�ꂽ�����ʒu�����̌������s
2133     While( MEndFlg% = 0 )
2134         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2135         MSetGrNumRetryExitFlg = 0
2136         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2137         While( MSetGrNumRetryExitFlg = 0 )
2138         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2139             '
2140             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2141             '
2142             '----- �����O���[�v�ԍ��ݒ� -----
2143             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2144             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2145             '
2146             '�����ʒu�ֈړ��E�ړ������҂�
2147             Mvs PInspPos( MNum% )                                       '�ړ�
2148             Dly 0.05                                                    '�ړ�������Delay
2149             '
2150             '�����O���[�v�ԍ��ݒ�I���m�F
2151             M_Timer(1) = 0
2152             MExitFlg = 0
2153             While( MExitFlg = 0 )
2154                 '����G�ݒ萳��I��?
2155                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2156                     MExitFlg = 1
2157                 '
2158                 '����G�ݒ�ُ�I��?
2159                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2160                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2161                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2162                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2163                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2164                     EndIf
2165                     MExitFlg = 1
2166                 '
2167                 'timeout�`�F�b�N
2168                 ElseIf 1000 < M_Timer(1) Then
2169                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2170                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2171                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2172                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2173                     EndIf
2174                     MExitFlg = 1
2175                 EndIf
2176             WEnd
2177             '
2178             '����G�ԍ��ݒ�v��off
2179             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2180             '
2181             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2182             'NG�Ȃ���Δ�����
2183             If MCurrentStepErr = 0 Then
2184                 MSetGrNumRetryExitFlg = 1
2185             Else
2186                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2187                 If MSetGrNumRetryCnt = 0 Then
2188                     MSetGrNumRetryExitFlg = 1
2189                 Else
2190                     'Retry�ց@���̑O��Delay
2191                     Dly 0.5
2192                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2193                 EndIf
2194             EndIf
2195             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2196             '
2197         WEnd
2198         '
2199         '
2200         '
2201         '----- �������s -----
2202         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2203             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2204                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2205                 MInspRetryExitFlg = 0
2206                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2207                 While( MInspRetryExitFlg = 0 )
2208                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2209                     '
2210                     '���������m�F
2211                     MRetryCnt = MRetryCnt - 1
2212                     M_Timer(1) = 0
2213                     MExitFlg = 0
2214                     While( MExitFlg = 0 )
2215                     '���������҂�
2216                         '����OK�I��?
2217                         If M_In( MIN_IS_InspOK% ) = 1  Then
2218                             MJudgeOKFlg = 1                         '����OK�t���OON
2219                             MExitFlg = 1
2220                         '
2221                         '����NG�I��?
2222                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2223                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2224                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2225                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2226                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2227                                 EndIf
2228                             EndIf
2229                             MExitFlg = 1
2230                         '
2231                         '�����ُ�I��(IS timeout)?
2232                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2233                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2234                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2235                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2236                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2237                                 EndIf
2238                             EndIf
2239                             MExitFlg = 1
2240                         '
2241                         'timeout�`�F�b�N
2242                         ElseIf 3000 < M_Timer(1) Then
2243                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2244                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2245                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2246                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2247                                 EndIf
2248                             EndIf
2249                             MExitFlg = 1
2250                         EndIf
2251                     WEnd
2252                     '
2253                     '�����J�n�v��off
2254                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2255                     '
2256                     'OK�Ȃ甲����
2257                     If MJudgeOKFlg = 1 Then
2258                         MInspRetryExitFlg = 1
2259                     Else
2260                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2261                         If MRetryCnt = 0 Then
2262                             MInspRetryExitFlg = 1
2263                         Else
2264                             'Retry�ց@���̑O��Delay
2265                             Dly 0.3
2266                         EndIf
2267                     EndIf
2268                     '
2269                 WEnd
2270             EndIf
2271         EndIf
2272         '
2273         '
2274         '
2275         MNum% = MNum% + 1                                           '����Step+1
2276         '�����I���m�F�@�����I���t���O�Z�b�g
2277         If (MInspCnt% < MNum% ) Then
2278             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2279         EndIf
2280         'NG���������s������
2281         If MInspErrNum <> 0 Then                                    'NG����?
2282             If MNgContinue% <> 1 Then                               'NG���s?
2283                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2284             EndIf
2285         EndIf
2286     WEnd
2287     '
2288     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2289     If 0 < MZAxis% Then
2290         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2291         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2292         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2293     EndIf
2294     Fine 0 , P
2295     '
2296     '�߂�l�ݒ�
2297     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2298         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2299     Else
2300         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2301         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2302         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2303     EndIf
2304     '
2305 FEnd
2306 '
2307 ' ��ISInspection
2308 ''' <summary>
2309 ''' Insight�ɂ��摜�����������s
2310 ''' </summary>
2311 '''<param name="PInspPos()">�����ʒu</param>
2312 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2313 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2314 '''<param name="MInspCnt%">�����ʒu��</param>
2315 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2316 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2317 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2318 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2319 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2320 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2321 ''' <remarks>
2322 ''' Date   : 2021/07/07 : M.Hayakawa
2323 ''' </remarks>
2324 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2325 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2326 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2327 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2328 '    EndIf
2329 ''
2330 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2331 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2332 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2333 '    Def Inte MEndFlg                                            '�����I���t���O
2334 '    MEndFlg% = 0
2335 '    '
2336 '    '�G���[�ԍ��N���A
2337 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2338 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2339 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2340 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2341 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2342 '    '
2343 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2344 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2345 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2346 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2347 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2348 ''
2349 '    EndIf
2350 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2351 '    '
2352 '    '�����ʒu���m�F
2353 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2354 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2355 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2356 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2357 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2358 ''
2359 '    EndIf
2360 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2361 '    '
2362 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2363 '    While( MEndFlg% = 0 )
2364 '        '�����I���m�F�@�����I���t���O�Z�b�g
2365 '        If (MInspCnt% < MNum% ) Then
2366 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2367 '        EndIf
2368 '        '
2369 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2370 '        If MEndFlg% = 0 Then
2371 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2372 '        EndIf
2373 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2374 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2375 '        '�^�X�N�@����G�ݒ�t���O���n��
2376 '        If MEndFlg% = 0 Then
2377 '            If 0 < MInspGrNum%(MNum%) Then
2378 '                M_03# = 1
2379 '            Else
2380 '                M_03# = 0
2381 '            EndIf
2382 '        Else
2383 '            M_03# = 0
2384 '        EndIf
2385 '        '�^�X�N�@�������ʊm�F�t���O���n��
2386 '        If 1 < MNum% Then
2387 '            If 0 < MInspGrNum%(MNum%-1) Then
2388 '                M_04# = 1
2389 '            Else
2390 '                M_04# = 0
2391 '            EndIf
2392 '        Else
2393 '            M_04# = 0
2394 '        EndIf
2395 '        '
2396 '        '�^�X�N�����J�n
2397 '        M_00# = 1                                               'TASK�����J�n
2398 '        '�^�X�N�����J�n�m�F
2399 '        M_Timer(1) = 0
2400 '        MExitFlg = 0
2401 '        While( MExitFlg = 0 )
2402 '            '�����J�n�����m�F
2403 '            If M_00# = 0 And M_10# = 8 Then
2404 '                MExitFlg = 1
2405 '            EndIf
2406 '            'timeout�`�F�b�N
2407 '            If 2000 < M_Timer(1) Then
2408 '                If MNgContinue% = 1 Then                        'NG���s?
2409 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2410 '                Else
2411 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2412 '                EndIf
2413 '                MExitFlg = 1
2414 '            EndIf
2415 '        WEnd
2416 '        '
2417 '        '�����ʒu�ֈړ��E�ړ������҂�
2418 '        If 0 = MInspErrNum Then
2419 '            If MEndFlg% = 0 Then
2420 '                Mvs PInspPos( MNum% )                           '�ړ�
2421 '            EndIf
2422 '        EndIf
2423 '        '
2424 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2425 '        If 0 = MInspErrNum Then
2426 '            M_Timer(1) = 0
2427 '            MExitFlg = 0
2428 '            While( MExitFlg = 0 )
2429 '                '���������҂��i����I���j
2430 '                If M_10# = 1 Then
2431 '                    MExitFlg = 1
2432 '                EndIf
2433 '                '���������҂��i�ُ�I���j
2434 '                If M_10# = 0 Then
2435 '                    If MNgContinue% = 1 Then                    'NG���s?
2436 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2437 '                    Else
2438 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2439 '                    EndIf
2440 '                    MExitFlg = 1
2441 '                EndIf
2442 '                'timeout�`�F�b�N
2443 '                If 5000 < M_Timer(1) Then
2444 '                    If MNgContinue% = 1 Then                    'NG���s?
2445 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2446 '                    Else
2447 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2448 '                    EndIf
2449 '                    MExitFlg = 1
2450 '                EndIf
2451 '            WEnd
2452 '        EndIf
2453 '        '
2454 '        '�������ʊm�F
2455 '        If 0 = MInspErrNum Then
2456 '            If 1 < MNum% Then
2457 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2458 '                    If M_11# = 2 Then                           '����NG?
2459 '                        If MNgContinue% = 1 Then                'NG���s?
2460 '                            If MInspNGStepNum = 0 Then          'NG������?
2461 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2462 '                            EndIf
2463 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2464 '                        Else
2465 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2466 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2467 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2468 '                        EndIf
2469 '                   EndIf
2470 '                EndIf
2471 '            EndIf
2472 '        EndIf
2473 '        '
2474 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2475 '        If 0 <> MInspErrNum Then
2476 '            MEndFlg% = 1
2477 '        EndIf
2478 '        '
2479 '        '�������s�A�捞�����҂�
2480 '        If 0 = MInspErrNum Then
2481 '            If MEndFlg% = 0 Then
2482 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2483 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2484 '                    '�捞�����m�F
2485 '                    M_Timer(1) = 0
2486 '                    MExitFlg = 0
2487 '                    While( MExitFlg = 0 )
2488 '                        '���������҂�
2489 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2490 '                            MExitFlg = 1
2491 '                        EndIf
2492 '                        'timeout�`�F�b�N
2493 '                        If 2000 < M_Timer(1) Then
2494 '                            If MNgContinue% = 1 Then            'NG���s?
2495 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2496 '                            Else
2497 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2498 '                            EndIf
2499 '                            MExitFlg = 1
2500 '                        EndIf
2501 '                    WEnd
2502 '                EndIf
2503 '                '
2504 '            EndIf
2505 '        EndIf
2506 '        MNum% = MNum% + 1
2507 '    WEnd
2508 '    '
2509 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2510 '    If 0 < MZAxis% Then
2511 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2512 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2513 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2514 '    EndIf
2515 '    '
2516 '    'NG���s������
2517 '    If MNgContinue% = 1 Then                                    'NG���s?
2518 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2519 '    EndIf
2520 '    '
2521 '    '�߂�l�ݒ�
2522 '    If MInspErrNum = 0 Then
2523 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2524 '    Else
2525 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2526 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2527 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2528 '    EndIf
2529 '    '
2530 '*ISInspection_End
2531 'FEnd
2532 '
2533 '��InitialZoneB
2534 ''' <summary>
2535 ''' ����~��̕��A����
2536 ''' 1)���ޔ��@Z������Ɉړ�
2537 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2538 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2539 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2540 ''' </summary>
2541 ''' <remarks>
2542 ''' Date : 2022/04/08 : N.Watanabe
2543 ''' </remarks>
2544 Function V fnInitialZoneB()
2545     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2546 '
2547 '�p�����[�^
2548     Ovrd 5
2549 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2550 '    Cmp Pos, &B100011
2551 '
2552 '���A����J�n
2553 '
2554 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2555 *RecoveryChuckOpen
2556     PActive = P_Curr          '���݈ʒu���擾
2557     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2558 'PProductOnRoboSet(�˂����{���i�u���ʒu)�́A�`���b�N���
2559     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2560         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2561             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2562                 MRecoveryChuckOpen = 1
2563             EndIf
2564         EndIf
2565     EndIf
2566 'PProductOnRoboGet(�˂����{���i���ʒu)�́A�`���b�N���
2567     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2568         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2569             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2570                 MRecoveryChuckOpen = 1
2571             EndIf
2572         EndIf
2573     EndIf
2574 '
2575     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2576     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2577     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2578 '
2579     M_20# = 0                                  'KEY���͏�����
2580     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2581     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2582     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2583 '
2584     fErrorProcess(11,244,284,0)
2585     If M_20# = MNext% Then M_20# = MClear%
2586     If M_20# = MAbout% Then GoTo *RecoveryEnd
2587     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2588     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2589 '
2590     *RecoveryChuckOpenEnd
2591 '
2592 '�w�ʔ��
2593 'PPlateBackSet�`PPlateBackSet_6�̃G���A�ɂ���Ƃ��́A�{�̃`���b�N�J��
2594 '�EPPlateBackSet_6         '�o�H6
2595 '�EPPlateBackSet_5         '�o�H7
2596 '�EPPlateBackSet_4         '�o�H8
2597 '�EPPlateBackSet_3         '�o�H9
2598 '�EPPlateBackSet_2         '�o�H10
2599 '�EPPlateBackSet_1         '�o�H11
2600 '�EPPlateBackSet           '�w�ʔu���ʒu
2601 '��L�V�_�̂w���W�E�x���W�E�y���W��J6�������LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2602     PActive = P_Curr                    '���݈ʒu���擾
2603     JActive = J_Curr                    '���݈ʒu���擾
2604     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2605     If (PActive.X >= -35) And (PActive.X <= -5) Then
2606         If (PActive.Y >= 350) And (PActive.Y <= 515) Then
2607             If (PActive.Z >= 480) And (PActive.Z <= 560) Then
2608                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2609                     M_Out(12256) = 0            '�{�̃`���b�N��OFF
2610                     M_Out(12257) = 1            '�{�̃`���b�N�JON
2611                 Dly 1.0
2612                 EndIf
2613             EndIf
2614         EndIf
2615     EndIf
2616 '
2617 '
2618 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2619 '
2620     Ovrd 1
2621 'PProductOnRoboSet(Get)�`PProductOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_2��
2622 '�EPProductOnRoboSet
2623 '�EPProductOnRoboSet_1
2624 '�EPProductOnRoboSet_2
2625 '�EPProductOnRoboGet
2626 '�EPProductOnRoboGet_1
2627 '�EPProductOnRoboGet_2
2628 '��L�U�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2629     PActive = P_Curr                    '���݈ʒu���擾
2630     JActive = J_Curr                    '���݈ʒu���擾
2631     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2632     If (PActive.X >= -30) And (PActive.X <= 0) Then
2633         If (PActive.Y >= 380) And (PActive.Y <= 420) Then
2634             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2635                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2636                     Mvs PProductOnRoboSet_1
2637                     Dly 1.0
2638                     Mvs PProductOnRoboSet_2
2639                     Dly 1.0
2640                     Mov PProductOnRoboSet_3
2641                     Dly 1.0
2642                 EndIf
2643             EndIf
2644         EndIf
2645     EndIf
2646 '
2647 'PProductOnRoboSet(Get)_2�`PProductOnRoboSet(Get)_3�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_3��
2648 '�EPProductOnRoboSet_2
2649 '�EPProductOnRoboSet_3
2650 '�EPProductOnRoboGet_2
2651 '�EPProductOnRoboGet_3
2652 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2653     PActive = P_Curr                    '���݈ʒu���擾
2654     JActive = J_Curr                    '���݈ʒu���擾
2655     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2656     If (PActive.X >= -35) And (PActive.X <= 0) Then
2657         If (PActive.Y >= 280) And (PActive.Y <= 400) Then
2658             If (PActive.Z >= 410) And (PActive.Z <= 530) Then
2659                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2660                     Mvs PProductOnRoboSet_3
2661                     Dly 1.0
2662                 EndIf
2663             EndIf
2664         EndIf
2665     EndIf
2666 '
2667     Ovrd 5
2668 '
2669 '���ޔ�
2670     PActive = P_Curr
2671     Pmove = PActive
2672     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
2673     If PActive.X > 550 Then
2674         Pmove.Z =550        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
2675     EndIf
2676     If PActive.Z < Pmove.Z Then
2677         Mvs Pmove
2678     EndIf
2679     Dly 1.0
2680 'J1���ȊO��ޔ��|�W�V�����ֈړ�
2681     JActive = J_Curr
2682     Jmove = JTaihi
2683     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2684     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2685     Mov Jmove
2686     Dly 1.0
2687 'J1���݂̂�ޔ��|�W�V�����ֈړ�
2688     Mov JTaihi
2689     Dly 1.0
2690 '�C�j�V�����|�W�V�����ֈړ�
2691     Mov PInitialPosition
2692     Cmp Off
2693     Ovrd 100
2694 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
2695     If M_In(11856) = 0 Then                 ' ��~���̂�
2696         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
2697         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
2698         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
2699         If MRet = 0 Then
2700         Else
2701             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
2702         EndIf
2703     EndIf
2704     M_Out(12262) = 0            '�ʒu���ߏoOFF
2705     M_Out(12263) = 1            '�ʒu���ߖ�ON
2706     fErrorProcess(11,253,281,0)
2707 *RecoveryEnd
2708     Exit Function
2709 FEnd
2710 '
2711 '
2712 '��fnAutoScreenComment
2713 ''' <summary>
2714 ''' ���C����ʂ̓���󋵕\��
2715 ''' �R�����gD1005�̐ݒ�
2716 ''' </summary>
2717 '''<param name="McommentD1005%">�R�����gID</param>
2718 ''' <remarks>
2719 ''' Date   : 2021/07/07 : M.Hayakawa
2720 ''' </remarks>
2721 Function fnAutoScreenComment(ByVal McommentD1005%)
2722     M_Out16(12576) = McommentD1005%
2723 FEnd
2724 '
2725 '��fnRoboPosChk
2726 ''' <summary>
2727 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2728 ''' </summary>
2729 '''<param name="MINNumber%">���͔ԍ�</param>
2730 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2731 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2732 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2733 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2734 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2735 ''' <remarks>
2736 ''' Date   : 2021/07/07 : M.Hayakawa
2737 ''' </remarks>
2738 Function M% fnRoboPosChk
2739     fnRoboPosChk = 0
2740     MRet = fnStepRead()
2741     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2742     '�E�B���h��ʐ؊���
2743     If MRBTOpeGroupNo > 5 Then
2744         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2745         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2746         Dly 0.2
2747         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2748         Dly 1.5
2749         '
2750         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2751         '
2752         MLoopFlg% = 1
2753         While MLoopFlg% = 1
2754             '
2755             '
2756             MKeyNumber% = fnKEY_WAIT()
2757             Select MKeyNumber%
2758                 Case Is = MAbout%       '��~
2759                     M_20# = MAbout%
2760                     MLoopFlg% = -1
2761                     Break
2762                 Case Is = MNext%        '����
2763                     'MLoopFlg% = -1
2764                     Break
2765                 Case Is = MContinue%    '�p��
2766                     M_20# = MContinue%
2767                     MLoopFlg% = -1
2768                     Break
2769                 Default
2770                     Break
2771             End Select
2772         WEnd
2773     EndIf
2774     '
2775     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2776         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2777         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2778         Select MRBTOpeGroupNo
2779             Case Is = 5                          '�������Ȃ�
2780                 Break
2781             Case Is = 10                         '�����ʒu�֖߂�
2782                 'Mov PTEST001
2783                 Break
2784             Case Is = 15                         '�����ʒu�֖߂�
2785                 'Mov PTEST002
2786                 Dly 0.5
2787                 'Mov PTEST001
2788                 Dly 0.5
2789                 Break
2790             Default
2791                 Break
2792         End Select
2793         '
2794         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2795         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2796         MRBTOpeGroupNo = 5
2797         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2798         Dly 1.0
2799         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2800         fnRoboPosChk = 1                        '�����ʒu������s
2801         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2802     EndIf
2803     Exit Function
2804 FEnd
2805 '
2806 '��frInCheck
2807 ''' <summary>
2808 ''' �Z���T�[IN�`�F�b�N
2809 ''' </summary>
2810 '''<param name="MINNumber%">���͔ԍ�</param>
2811 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2812 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2813 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2814 ''' <remarks>
2815 ''' Date   : 2021/07/07 : M.Hayakawa
2816 ''' </remarks>
2817 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2818     M_Timer(4) = 0
2819     MloopFlg = 0
2820     While MloopFlg = 0
2821         MCrtTime& = M_Timer(4)
2822         If M_In(MINNumber%) = MCMPFLG% Then
2823             MloopFlg = 1
2824             frInCheck = 1
2825         ElseIf MCrtTime& > MTimeCnt& Then
2826             MloopFlg = 1
2827             frInCheck = 0
2828         EndIf
2829     WEnd
2830 FEnd
2831 '-----------------------------------------------
2832 '
2833 '�˂����ߋ@�ʐM�m�F
2834 '
2835 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2836 'fScrewTcomChk = 0�@�F����I��
2837 '          �@ �@ -1 �F�ُ�I��
2838 '-----------------------------------------------
2839 Function M% fScrewTcomChk
2840 *ReCheckScewTcomChk
2841     fScrewTcomChk = 0
2842     '�ʐM�m�F���M
2843     M_Out(MOUT_ScwT_ComChk%) = MOn%
2844     '�ʐM�m�F��M�ҋ@
2845 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2846     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2847     '�ʐM�m�F���M�I��
2848     M_Out(MOUT_ScwT_ComChk%) = MOff%
2849     If MRtn = 0 Then
2850         fScrewTcomChk = -1
2851     EndIf
2852     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2853  '
2854 FEnd
2855 '
2856 '
2857 '-----------------------------------------------
2858 '
2859 '�˂����ߊJ�n���M
2860 '
2861 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2862 'fScrewTStart = 0�@�F����I��
2863 '           �@�@-1 �F�ُ�I��
2864 '-----------------------------------------------
2865 Function M% fScrewTStart
2866     fScrewTStart = 0
2867     nRet% = 0
2868     '�˂����ߊJ�n�ҋ@����M
2869 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2870     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2871     If MRtn = 0 Then nRet% = -1
2872     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
2873     Dly 0.1
2874     '�˂����ߊJ�n��M�𑗐M
2875     M_Out(MOUT_ScwT_ST%) = MOn%
2876     Dly 0.5
2877     'Wait M_In(MTEST_KEY%) = MOn%
2878     '�˂����ߊJ�n���M�I��
2879     M_Out(MOUT_ScwT_ST%) = MOff%
2880     '
2881 *ScrewStartERROR
2882     fScrewTStart = nRet%
2883 FEnd
2884 '
2885 '
2886 '
2887 '-----------------------------------------------
2888 '
2889 '�˂����ߊ�����M
2890 '
2891 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2892 'fScewTFinish = 0�@�F����I��
2893 '          �@ �@-1 �F�ُ�I��
2894 '-----------------------------------------------
2895 Function M% fScewTFinish
2896 *ReCheckScewTFinish
2897     fScewTFinish = 0
2898     '�˂����ߊ����ҋ@����M
2899 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
2900     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
2901     If MRtn = 0 Then
2902         fScewTFinish = -1
2903     EndIf
2904     If MRtn = 2 Then GoTo *ReCheckScewTFinish
2905     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
2906     Dly 0.1
2907     '�˂����ߊ�����M�𑗐M
2908     M_Out(MOUT_ScwT_FinOK%) = MOn%
2909     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
2910     '�˂����ߊJ�n���M�I��
2911     M_Out(MOUT_ScwT_FinOK%) = MOff%
2912     'Wait M_In(MTEST_KEY%) = MOn%
2913     '
2914 *ScewTFinish_ErrEnd
2915 FEnd
2916 '
2917 '
2918 '-----------------------------------------------
2919 '
2920 '����xx��~��M
2921 '
2922 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2923 'fScewTCaseStop = 0�@�F����I��
2924 '          �@   �@-1 �F�ُ�I��
2925 '-----------------------------------------------
2926 Function M% fScewTCaseStop(ByVal MCase%())
2927 *ReCheckScewTCaseStop
2928     fScewTCaseStop = 0
2929     '����xx��~����M
2930     Wait M_In(MCase%(1)) = MOn%
2931     MRtn = fTimeOutJudge(MCase%(1),MOn%)
2932     If MRtn = 0 Then
2933         fScewTCaseStop = -1
2934     EndIf
2935     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
2936     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
2937     Dly 0.1
2938     '����xx��~��M�𑗐M
2939     M_Out(MCase%(2)) = MOn%
2940     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
2941     '�˂����ߊJ�n���M�I��
2942     M_Out(MCase%(2)) = MOff%
2943 *ScewTCaseStop_ErrEnd
2944     '
2945 FEnd
2946 '
2947 '��fScrewTighenRoboCheck
2948 '<summary>
2949 '�˂����{�Ď�
2950 '</summary>
2951 '<param name = "MStopNum%"> ��~�ԍ�</param>
2952 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
2953 '<make>
2954 '2021/12/2 �����V��
2955 '</make>
2956 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
2957     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
2958     fScrewTighenRoboCheck = 1
2959     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
2960     MCheck% = 0
2961     While MScrewTighenRoboFlg% = 1
2962         MCheck% = M_In16(11904)
2963         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
2964             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
2965             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
2966         EndIf
2967         If MCheck% <> 0 Then
2968             fScrewTighenRoboError(MCheck%)
2969             Select M_20#
2970                 Case MAbout%            '��~�������ꂽ�ꍇ
2971                     M_Out(12869) = 1 Dly 1.0
2972                     MScrewTighenRoboFlg% = 0
2973                     fScrewTighenRoboCheck = 0   '�ُ�I��
2974                     Break
2975                 Case MNgProcess%        'NG�������ꂽ�ꍇ
2976                     M_Out(12873) = 1 Dly 1.0
2977                     MScrewTighenRoboFlg% = 0
2978                     fScrewTighenRoboCheck = 0   '�ُ�I��
2979                     Break
2980                 Case MContinue%             '���g���C�������ꂽ�ꍇ
2981                     M_20# = MClear%         'M_20#������
2982                     M_Out(12871) = 1 Dly 1.0
2983                     Break
2984                 Case MNext%                 '���ւ������ꂽ�ꍇ
2985                     M_20# = MClear%         'M_20#������
2986                     M_Out(12874) = 1 Dly 1.0
2987                     Break
2988             End Select
2989             Dly 0.5
2990         EndIf
2991     WEnd
2992 FEnd
2993 '
2994 '��fScrewTighenRoboError
2995 '<summary>
2996 '�˂����{�G���[����
2997 '</summary>
2998 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
2999 '<make>
3000 '2021/12/2 �����V��
3001 '</make>
3002 Function fScrewTighenRoboError(ByVal MErrorCode%)
3003     MErrorScreenCode% = 0
3004     MErrorScreenCode% = MErrorCode% + 300
3005     fErrorProcess(11,MErrorScreenCode%,0,0)
3006 FEnd
3007 '
3008 '��fErrorProcess
3009 '<summary>
3010 '�G���[����
3011 '</summary>
3012 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3013 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3014 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3015 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3016 '<make>
3017 '2021/11/5 �����V��
3018 '</make>
3019 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3020     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3021     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3022     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3023     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3024 *RETRY_ERR_PROCESS
3025      M_20# = MClear%     '������
3026 '        '�G���[�����L�q
3027         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3028 '        'GOT KEY���͑҂�
3029         MKeyNumber = fnKEY_WAIT()
3030 '        '
3031         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3032             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3033             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3034             Break
3035          '
3036         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3037             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3038             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3039         '
3040         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3041             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3042             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3043          '
3044         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3045             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3046             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3047             Break
3048         '
3049         EndIf
3050         '
3051         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3052 FEnd
3053 '
3054 '��fnTorqueCheck
3055 ''' <summary>
3056 ''' �g���N�`�F�b�N����p�̃��C��
3057 ''' </summary>
3058 ''' <remarks>
3059 ''' Date   : 2021/12/21 : H.AJI
3060 ''' </remarks>'
3061 Function M% fnTorqueCheck
3062     '�g���N�`�F�b�N�����M  �����n��~
3063     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3064     '
3065     fnTorqueCheck = 0
3066     Ovrd 20
3067     Mov PInitialPosition              '�����ʒu�ړ�
3068     Ovrd 100
3069     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3070     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3071     Dly 0.2
3072     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3073     '
3074     'M6340  �g���N�`�F�b�N��M
3075     'Dly 5.0
3076     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3077     Dly 1.0
3078     M_Out(12340) = 0
3079     '
3080     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3081     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3082    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3083     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3084     '
3085     '
3086     MLoopFlg = 1
3087     While MLoopFlg = 1
3088         '
3089         Mov PInitialPosition              '�����ʒu�ړ�
3090         '
3091         MKeyNumber = fnKEY_WAIT()
3092         Select MKeyNumber
3093             Case Is = 1           '��~
3094                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3095                 Dly 1.0
3096                 M_Out(12343) = 0
3097                 Ovrd 20
3098                 'Mov PTicketRead_1
3099                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3100                 Wait M_In(11859) = 1      '�˂����{����̏I��
3101                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3102                 Ovrd 100
3103                 M_20# = 1
3104                 MLoopFlg = -1
3105                 Break
3106             Case Is = 2           '����
3107                 Break
3108             Case Is = 3           '�p��
3109                 Break
3110             Case Is = 4           '�g���N�`�F�b�N�J�n
3111                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3112                 Dly 1.0
3113                 M_Out(12342) = 0
3114                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3115                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3116                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3117                 EndIf
3118                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3119                 'MRet = fnMoveTorquePosi()
3120                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3121                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3122                 Break
3123             Default
3124                 Break
3125         End Select
3126     WEnd
3127     '
3128     '�g���N�`�F�b�N����~���M
3129     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3130     '
3131     '���{�b�g�̈ʒu�����ɖ߂�
3132     '
3133     '
3134  FEnd
3135  '
3136 '
3137 '
3138 '---------------------------
3139 '
3140 '    ���C����ʂ̕\���A��\���ݒ�
3141 '         �R�����gD1001, D1002, D1003�̐ݒ�
3142 '           MWindReSet = 0     ��ʔ�\��
3143 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3144 '           MWindErrScr = 10    �G���[��� D1001, D1002
3145 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3146 '
3147 '---------------------------
3148 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3149     fnMainScreenOpen = 0
3150     '
3151    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3152         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3153     EndIf
3154     '
3155     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3156         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3157     EndIf
3158     '
3159     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3160         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3161     EndIf
3162     '
3163     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3164     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3165     Dly 0.5
3166     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3167 FEnd
3168 '
3169 '��Main
3170 ''' <summary>
3171 ''' �g���N�`�F�b�N������
3172 ''' </summary>
3173 ''' <remarks>
3174 ''' Date   : 2021/12/21 : H.AJI
3175 ''' </remarks>'
3176 Function M% fnScrewMTorque
3177     fnScrewMTorque = 0
3178     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3179     Wait M_In(11857) = 1                     '��M����
3180     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3181     Dly 2.0
3182 FEnd
3183 '
3184 '
3185 '----------------------------------------------------------------
3186 'fTimeOutJudge
3187 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3188 '����
3189 'Address% = �Ď��A�h���X�ԍ�
3190 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3191 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3192 '�߂�l = 0 �G���[
3193 '         1 ����I��
3194 '         2 ���g���C
3195 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3196 '�쐬��
3197 '2022/9/20 ����
3198 '----------------------------------------------------------------
3199 '
3200 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3201     fTimeOutJudge = 0
3202     MJudge% = 1
3203     MRtn = 0
3204     M_20# = MClear%
3205     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3206 *TimeOutLoop
3207     If MRtn = 1 Then GoTo *TimeOut
3208         fErrorProcess(11,202,203,0)
3209         If M_20# = MNext% Then GoTo *TimeOutLoop
3210         If M_20# = MContinue% Then MJudge% = 2
3211         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3212 *TimeOut
3213     fTimeOutJudge = MJudge%
3214 '
3215 *JUDGE_ERROR_END
3216 FEnd
3217 '��Main
3218 ''' <summary>
3219 ''' �g������p�̃��C��
3220 ''' </summary>
3221 ''' <remarks>
3222 ''' Date   : 2021/07/07 : M.Hayakawa
3223 ''' </remarks>'
3224 Function Main
3225     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3226     '
3227     If M_Svo=0 Then
3228         Servo On
3229     EndIf
3230     Wait M_Svo=1
3231 '�g���X�^�[�g���t�����v���p���XON
3232     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3233 '�p�g���C�g����
3234     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3235     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3236     '
3237     M_20# = 0                                   'KEY���͏�����
3238     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3239     MRet% = 0
3240 '�����ʒu�̊m�F�ƈړ�
3241 '
3242 '���A����@���s�E�����s����      2022/04/08 �n�� �쐬
3243     PActive = P_Curr                    '���݈ʒu���擾
3244     MRecoveryPass% = 0
3245     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3246         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3247             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3248                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3249             EndIf
3250         EndIf
3251     EndIf
3252     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3253         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3254             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3255                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3256             EndIf
3257         EndIf
3258     EndIf
3259     If MRecoveryPass% = 0 Then
3260        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3261     EndIf
3262 '
3263     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3264         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3265 '�g���N�`�F�b�N
3266         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3267             MRet% = fnTorqueCheck()
3268             Break
3269         Else
3270 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3271 '                MRtn = InspInit()               '�摜��������������
3272 '            EndIf
3273 '
3274             M_20# = MClear%             '������
3275 '�g���J�n
3276             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3277                 fnAssyStart()
3278             Else
3279                 M_20# = MPass%
3280             EndIf
3281 '�g���I�����t����
3282             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3283             Wait M_In(11572) = 1            '���t�擾����
3284             Dly 0.1
3285             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3286 '���t�^�[���j�b�g�ւ�OUT
3287             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3288             fnAutoScreenComment(89)         'AUTO��� �g����������
3289             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3290 'OK/NG�t���O�o��
3291             If M_20# <= 0 Then
3292                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3293             ElseIf M_20# = MPass% Then
3294                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3295             EndIf
3296 'PIAS�ɑg������������
3297             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3298                 If M_20# = MPass% Then
3299                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3300                 Else
3301                     'KEY���͂�NG�̏ꍇ
3302                     If M_20# = MNgProcess% Then
3303                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3304                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3305                         MRet% = fnPiasWrite(MNG%)
3306                        nAssyNgQty = nAssyNgQty + 1
3307                     EndIf
3308                     '
3309                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3310                     If M_20# = MAssyOK% Then
3311                             '-----------------------
3312                             'D732 -> D2600 �R�s�[�v��
3313                             M_Out(12566) = 1
3314 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3315                             M_Out(12566) = 0
3316                             '
3317                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3318                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3319                             '��ԍ��ƍ�(PP�͖��g�p�j
3320 '                            MRet% = fnPCBNumberCheck()
3321                         Else
3322                             MRet% = 1
3323                         EndIf
3324                         '
3325                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3326                             If M_20# <> MAbout% Then
3327                                 '�H������OK��������
3328                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3329                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3330                                 MRet% = fnPiasWrite(MOK%)
3331                                 nAssyOkQty = 0
3332                                 nAssyOkQty = nAssyOkQty + 1
3333                             Else
3334                                 nAssyOkQty = nAssyOkQty + 1
3335                             EndIf
3336                         EndIf
3337                     EndIf
3338 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3339 '                    MRet% = fnPiasWrite(MOK%)
3340                 EndIf
3341             Else
3342                 nAssyOkQty = nAssyOkQty + 1
3343             EndIf
3344             '
3345             '�g���I�����t��������
3346             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3347             '�������A�g��OK���A�g��NG��������
3348 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3349             '
3350 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3351 '                '�摜�����I������
3352 '                MRtn = InspQuit()
3353 '            EndIf
3354         EndIf
3355         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3356     EndIf
3357 '�p�g���C�g����
3358     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3359     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3360 'GOT�\��
3361     fnAutoScreenComment(93)  'AUTO��� �H������
3362 FEnd
3363 End
3364 '
3365 '���܂��Ȃ��R�����g
3366 '��΍폜�����
3367 '
3368 '
3369 '
3370 '
3371 '
3372 '
3373 '
PInspPosition(1)=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(11)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(12)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(13)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(14)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(15)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(16)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(17)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(18)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(19)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(20)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(21)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(22)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(23)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(24)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(25)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(26)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(27)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(28)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(29)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(30)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTemp=(+602.00,-150.00,+550.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PActive=(+602.00,-150.00,+550.00,-180.00,+0.00,+90.00)(7,0)
Pmove=(-214.29,+565.14,+640.00,+179.97,-0.02,+4.00)(7,0)
PInitialPosition=(+340.00,+0.00,+580.00,-180.00,+0.00,+180.00)(7,0)
PMechaGet=(-418.66,-2.92,+305.03,+180.00,+0.00,-179.99)(7,1048577)
PMechaGet_1=(-418.66,-2.92,+410.00,+180.00,+0.00,-179.99)(7,1048577)
PMechaGet_2=(-189.84,-0.01,+629.06,-180.00,+0.00,-179.99)(7,1)
PMechaGet_3=(+0.01,+189.84,+629.07,-180.00,+0.00,+90.00)(7,0)
PMechaGet_4=(+327.50,+0.02,+596.24,-179.99,+0.00,+103.50)(7,0)
PMechaGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet1=(+167.10,-331.10,+318.57,-87.20,+88.18,-177.23)(6,0)
PMechaSet1_1=(+167.10,-331.10,+340.00,-87.20,+88.18,-177.23)(6,0)
PMechaSet2=(+169.45,-331.62,+319.06,-89.34,+88.05,-179.87)(6,0)
PMechaSet2_1=(+169.45,-331.62,+340.00,-89.34,+88.05,-179.87)(6,0)
PMechaSet_2=(+162.58,-305.37,+557.38,+179.47,+90.00,+89.47)(6,0)
PMechaSet_3=(+114.45,-288.22,+565.58,+180.00,+0.00,+112.11)(7,0)
PMechaSet_4=(+310.11,-0.04,+565.56,+180.00,+0.00,-179.55)(7,0)
PMechaSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck=(-90.21,+513.03,+577.72,-180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_2=(+66.39,+429.86,+577.75,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_3=(-18.78,+286.22,+630.88,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet=(+478.13,+103.51,+401.67,+179.65,+0.13,-179.14)(7,0)
PPlateBackGet_1=(+478.13,+103.51,+430.00,+179.65,+0.13,-179.14)(7,0)
PPlateBackGet_2=(+478.13,+103.51,+560.00,+179.65,+0.13,-179.14)(7,0)
PPlateBackGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackPush=(-20.68,+418.34,+540.82,-180.00,+0.00,+90.51)(7,1048576)
PPlateBackPush_1=(-20.68,+400.00,+540.82,-180.00,+0.00,+90.51)(7,1048576)
PPlateBackPush_2=(-20.68,+380.00,+564.00,+180.00,+0.00,+90.00)(7,1048576)
PPlateBackSet=(-21.30,+459.99,+540.88,+179.43,-11.00,+90.80)(7,1048576)
PPlateBackSet_00=(-20.61,+498.13,+545.38,+179.77,+0.00,+90.43)(7,1048576)
PPlateBackSet_1=(-21.30,+452.59,+539.57,+179.40,-13.00,+90.81)(7,1048576)
PPlateBackSet_10=(-21.27,+354.31,+478.81,+179.08,-44.98,+90.81)(7,1048576)
PPlateBackSet_11=(-21.24,+351.47,+478.81,+179.08,-44.98,+90.91)(7,1048576)
PPlateBackSet_12=(-20.87,+345.00,+495.00,+179.08,-44.99,+90.43)(7,1048576)
PPlateBackSet_13=(-17.88,+286.22,+630.90,-179.82,-0.29,+90.49)(7,1048576)
PPlateBackSet_2=(-21.30,+439.30,+535.46,+179.35,-17.00,+90.82)(7,1048576)
PPlateBackSet_3=(-21.30,+425.40,+530.66,+179.29,-21.00,+90.84)(7,1048576)
PPlateBackSet_4=(-21.30,+413.50,+524.45,+179.22,-25.00,+90.87)(7,1048576)
PPlateBackSet_5=(-21.30,+399.71,+518.44,+179.15,-29.00,+90.90)(7,1048576)
PPlateBackSet_6=(-21.30,+388.51,+509.33,+179.08,-33.00,+90.94)(7,1048576)
PPlateBackSet_7=(-21.30,+377.51,+502.43,+179.00,-37.00,+90.98)(7,1048576)
PPlateBackSet_8=(-21.30,+366.41,+492.73,+178.90,-41.00,+91.04)(7,1048576)
PPlateBackSet_9=(-21.30,+356.07,+482.09,+179.08,-44.98,+91.11)(7,1048576)
PProductOnPltGet=(+479.40,-99.73,+372.85,+179.99,-0.35,-179.43)(7,0)
PProductOnPltGet_1=(+479.40,-99.73,+410.00,+179.99,-0.35,-179.43)(7,0)
PProductOnPltGet_2=(+479.40,-99.73,+510.00,+179.99,-0.35,-179.43)(7,0)
PProductOnPltGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet=(+478.90,-99.73,+372.85,+179.99,-0.35,-179.43)(7,0)
PProductOnPltSet_1=(+478.90,-99.73,+410.00,+179.99,-0.35,-179.43)(7,0)
PProductOnPltSet_2=(+478.90,-99.73,+510.00,+179.99,-0.35,-179.43)(7,0)
PProductOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet=(-19.85,+405.99,+321.18,-111.55,+88.83,-21.44)(6,0)
PProductOnRoboGet_1=(-19.85,+405.99,+425.20,-112.91,+88.92,-23.00)(6,0)
PProductOnRoboGet_2=(-19.85,+387.42,+425.20,-112.91,+88.92,-22.80)(6,0)
PProductOnRoboGet_3=(-17.16,+300.00,+550.00,-66.19,+88.98,+23.82)(6,0)
PProductOnRoboGet_4=(-18.16,+300.00,+550.00,+175.04,+89.99,-94.95)(6,0)
PProductOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet=(-19.85,+405.99,+321.18,-111.55,+88.83,-21.44)(6,0)
PProductOnRoboSet_1=(-19.85,+405.99,+425.20,-112.91,+88.92,-22.80)(6,0)
PProductOnRoboSet_2=(-19.85,+387.42,+425.20,-112.91,+88.92,-23.00)(6,0)
PProductOnRoboSet_3=(-17.16,+300.00,+550.00,-66.19,+88.98,+23.82)(6,0)
PProductOnRoboSet_4=(-18.86,+404.69,+360.00,-102.74,+89.08,-12.36)(6,0)
PProductOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPushTilt=(-214.29,+565.14,+463.69,+179.97,-0.02,+4.00)(7,0)
PPushTilt_1=(-214.29,+565.14,+479.62,+179.97,-0.02,+4.00)(7,0)
PPushTilt_2=(-214.29,+565.14,+620.00,+179.97,-0.02,+4.00)(7,0)
PPushTilt_3=(+0.02,+340.00,+610.00,-180.00,-0.01,-91.91)(7,0)
PTicketRead=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.00,+550.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+110.77,+45.12,+39.40,-0.01,+95.51,-73.23)
Jmove=(+110.77,-46.87,+111.64,+0.00,+80.58,-73.23,+0.00,+0.00)
JTaihi=(+0.00,-46.87,+111.64,+0.00,+80.58,+0.00,+0.00,+0.00)
