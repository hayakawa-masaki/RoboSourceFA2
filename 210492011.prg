1 ' ===================================
2 '
3 '  21049001 STEP5 Assy1�v���O����
4 '
5 ' �쐬�ҁF������T
6 ' �쐬���F2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1���痬�p
8 ' Ver 0.3 2021.12.22 �摜�����֐�ISInspection��ISInspectionSingle�A�摜�����ǉ� file:210542003
9 ' ===================================
10 '===== <Insight�萔> =====
11 '===== <Insight�ϐ���`> =====
12 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
13 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
14 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
15 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
16 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
17 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
18 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
19 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
20 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
21 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
22 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
23 '
24 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
25 'Output Signal
26 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
27 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
28 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
29 '
30 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
31 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
32 '
33 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
34 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
35 '��Ɨp�ϐ�
36 Def Inte MInspErrNum                '�������s�G���[�ԍ�
37 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
38 Def Inte MRtn                       'Function�߂�l�擾�p
39 Def Inte MRtn2                      'Function�߂�l�擾�p
40 Def Inte MRet3                      'Function�߂�l�擾�p
41 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
42 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
43 Def Inte MovrdA                     '�l�W����Ovrd �ϗp   20191127�ǉ�
44 Def Float MSpdA                     '�l�W����Spd�@�ϗp   20191127�ǉ�
45 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p    20200312�ǉ�'
46 MovrdA% = 20                        '�l�W����Ovrd �ϗp   20191127�ǉ�
47 MSpdA = 800                        '�l�W����Spd�@�ϗp   20191127�ǉ�
48 '===== <Insight�ϐ��ݒ�> =====
49 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
50 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
51 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
52 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
53 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
54 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
55 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
56 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
57 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
58 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
59 'Output Signal
60 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
61 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
62 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
63 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
64 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
65 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
66 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
67 '===== <�d�h���萔> =====
68 '===== <�d�h���ϐ���`> =====
69 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
70 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
71 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
72 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
73 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
74 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
75 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
76 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
77 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
78 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
79 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
80 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
81 Y60_Driver=12240 '�d�h�������v��� CCW
82 Y61_Driver=12241 '�d�h�����v��� CW
83 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
84 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
85 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
86 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
87 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
88 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
89 '�g��2
90 X34_NG1=11268 '�˂�����1�@Read
91 X35_NG2=11269 '�˂�����2�@Read
92 '�g��3
93 X3F_NG1=11279 '�˂�����1�@Read
94 '
95 Dim PScrewPosTemp(10)                                               '�l�W���ߗpFunction�����ϐ�
96 Dim PGetScrewPosTemp(10)                                            '�˂������@����˂��𓾂�Function�����ϐ�
97 Dim PEscapePosi(10)
98 MLoopCnt% = 0'
99 '===== <���{�b�g�萔> =====
100 '===== <���{�b�g�ϐ���`> =====
101 MRBTOpeGroupNo = 0                    '���{�b�g����ԍ�������
102 MCommentD1001 = 0
103 MCommentD1002 = 0
104 MCommentD1003 = 0
105 MScreenNo = 0
106 '
107 MCommentTSU = 0
108 MCommentTSD = 0
109 '�E�B���h��ʔԍ��ݒ�
110 MWindReSet = 0
111 MWindInfoScr = 5
112 MWindErrScr = 10
113 MWindErrScr2 = 11
114 MWindErrScr3 = 13
115 MWindErrScr17 = 17
116 MWindErrScr18 = 18
117 MWindCmmnScr = 20
118 MWindJigRelase19049 = 60
119 MWindJigRelase19050 = 61
120 MWindJigRelase19051 = 62
121 '
122 MClear% = 0        'KEY_�̃N���A
123 MAbout% = 1        'KEY_��~
124 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
125 MContinue% = 3     'KEY_�p�� �ēx����������s��
126 '
127 Def Inte MNgProcess
128 MNgProcess% = 5      'KEY_NG
129 '
130 MAssyOK% = 6       '�g������
131 MPass% = 7         '�H���p�X
132 MPiasNG% = 8       'Pias�m�F������NG
133 '
134 '�������pKEY�ԍ�   '
135 MRobotInit1% = 11  '�����ʒu�p
136 MRobotInit2% = 12  '�����ʒu�p
137 MRobotInit3% = 13  '�����ʒu�p
138 MRobotInit4% = 14  '�����ʒu�p
139 '
140 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
141 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
142 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
143 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
144 '
145 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
146 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
147 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
148 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
149 '
150 MopeNo = 0
151 '
152 MOK% = 1               '�e����p
153 MNG% = 0               '�e����p
154 MTIMEOUT% = -1         '�e����p
155 MJudge% = 0            '������i�[�p
156 '
157 '
158 MRECIVETIME& = 0
159 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
160 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
161 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
162 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
163 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
164 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
165 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
166 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
167 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
168 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
169 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
170 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
171 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
172 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
173 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
174 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
175 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
176 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
177 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
178 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
179 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
180 MIN_PIAS_MyProcessComp% = 11573        '���H����������
181 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
182 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
183 '
184 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
185 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
186 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
187 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
188 '
189 MOUT_PiasAssyResultOK% = 12549    '�g��OK
190 MOUT_PiasAssyResultNG% = 12550    '�g��NG
191 MOUT_PiasAssyResultWr% = 12548    '�H��������������
192 '
193 MIN_PiasProcessNG% = 11559        '�H����������NG
194 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
195 MIN_PiasProcessOK% = 11558        '�H����������OK
196 '
197 MIN_Insight_Use% = 11369               '�摜�m�FON
198 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
199 '
200 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
201 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
202 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
203 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
204 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
205 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
206 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
207 '
208 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
209 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
210 '
211 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
212 '
213 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
214 '
215 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
216 MRtn% = 0
217 MopeNo = 0
218 MRet = 0
219 'MRtn = 0
220 MRet3% = 0
221 '
222 Def Inte MInputQty          '������ ���Z�ϐ�
223 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
224 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
225 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
226 Def Inte nAssyOkQty         '���g�p
227 Def Inte MScrewNo
228 Def Inte MReTry
229 '===== <IO�ϐ���`> =====
230 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
231 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
232 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
233 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
234 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
235 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
236 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
237 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
238 '
239 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
240 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
241 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
242 '
243 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
244 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
245 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
246 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
247 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
248 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
249 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
250 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
251 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
252 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
253 '
254 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
255 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
256 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
257 '
258 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
259 '
260 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
261 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
262 '
263 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
264 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
265 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
266 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
267 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
268 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
269 '
270 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
271 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
272 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
273 '
274 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
275 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
276 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
277 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
278 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
279 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
280 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
281 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u 'Y68_VV1% = 12250��Y68_VV1% = 12248�ɕύX(8/27����)
282 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u'Y6B_VB1% = 12251��Y6B_VB1% = 12250�ɕύX(8/27����)
283 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
284 '
285 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
286 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
287 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
288 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
289 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
290 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
291 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
292 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
293 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
294 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
295 '
296 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
297 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
298 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
299 '
300 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
301 '
302 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
303 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
304 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
305 '
306 '
307 '����
308 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
309 Def Inte MOn                            '�o��=1
310 Def Inte MOff                           '�o��=0
311 '
312 '�˂����ߑ��u_�o�̓A�h���X
313 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
314 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
315 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
316 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
317 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
318 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
319 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
320 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
321 '�˂����ߑ��u_���̓A�h���X
322 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
323 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
324 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
325 Def Inte MIN_ScwT_Case1                 '����1��~����M
326 Def Inte MIN_ScwT_Case2                 '����2��~����M
327 Def Inte MIN_ScwT_Case3                 '����3��~����M
328 Def Inte MIN_ScwT_Case4                 '����4��~����M
329 Def Inte MIN_ScwT_Case5                 '����5��~����M
330 '
331 Def Inte MRetryLimit                    ' ���g���C��
332 Def Inte MRetryCount                    ' ���g���C�J�E���g
333 '
334 Dim MScwT_Case1%(2)               '����1��~�ϐ�
335 Dim MScwT_Case2%(2)               '����2��~�ϐ�
336 Dim MScwT_Case3%(2)               '����3��~�ϐ�
337 Dim MScwT_Case4%(2)               '����4��~�ϐ�
338 Dim MScwT_Case5%(2)               '����5��~�ϐ�
339 '
340 Def Pos PActive                     '�������W�n �ʒu�ϐ� ���݈ʒu
341 Def Pos Pmove                       '�������W�n �ʒu�ϐ� �ړ���
342 Def Inte MRecoveryPass              '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s'
343 '����
344 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
345 MOn% = 1                                 '�o�� = 1
346 MOff% = 0                                '�o�� = 0
347 '
348 '�˂����ߋ@_�A�h���X�ݒ�
349 MOUT_ScwT_ComChk% = 12816               '�ʐM�m�F���M
350 MOUT_ScwT_ST% = 12849                   '�˂����ߊJ�n�𑗐M
351 MOUT_ScwT_ReSTOK% = 12850               '�ĊJ�n��M�𑗐M
352 MOUT_ScwT_FinOK% = 12852                '�˂����ߊ�����M�𑗐M
353 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
354 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
355 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
356 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
357 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
358 '
359 MIN_ScwT_comOK% = 11824                 '�˂����ߑ��u����ԐM
360 MIN_ScwT_STRec% = 11857                 '�˂����ߊJ�n����M
361 MIN_ScwT_ReST% = 11858                  '�ĊJ�n����M
362 MIN_ScwT_Fin% = 11860                   '�˂����ߊ�������M
363 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
364 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
365 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
366 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
367 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
368 '
369 MScwT_Case1%(1) = MIN_ScwT_Case1%
370 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
371 MScwT_Case2%(1) = MIN_ScwT_Case2%
372 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
373 MScwT_Case3%(1) = MIN_ScwT_Case3%
374 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
375 MScwT_Case4%(1) = MIN_ScwT_Case4%
376 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
377 MScwT_Case5%(1) = MIN_ScwT_Case5%
378 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
379 '
380 '
381 PCalcGetMainScrew = (+0.00,+0.00,-1.20,+0.00,+0.00,+0.00,+0.00)  'Main�˂������@�̕␳�l
382 PCalcGetFanScrew = (+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)   'Fan�˂������@�̕␳�l
383 '
384 MRetryLimit% = 2
385 '
386 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
387 Function M% fnAssyStart
388     M_20# = MClear%                       '������
389     '�g�ݗ��ĊJ�n
390     '�v���O�������_
391     Ovrd 100
392     ' �����ʒu��ID�`�P�b�g��Ƃ��邽�ߍ폜 9/16 M.Hayakawa
393 '    Mov PInitialPosition        '���_���
394     '�����ʒu��ݒ�
395     PTemp = P_Curr
396     MRtn = 0
397     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
398         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
399             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
400                 MRtn = 1
401                 Break
402             EndIf
403             Break
404         EndIf
405         Break
406     EndIf
407     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
408     If MRtn = 1 Then
409         M_Out(12256) = 1 Dly 0.3            '�ʒu���ߏoON
410         Mov PTicketRead
411         Break
412     Else
413         Mov PInitialPosition
414         M_Out(12256) = 1 Dly 0.3           '�ʒu���ߏoON
415         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
416         Mvs PTicketRead             'ID�ǂ݈ʒu
417         Break
418     EndIf
419     *RE_PUSH
420 '    If M_20# = MContinue% Then M_Out(12257) = 0
421     If M_20# = MContinue% Then M_Out(12256) = 1 Dly 0.3
422     If M_20# = MContinue% Then M_20# = MClear%
423     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
424     If MRtn = 1 Then GoTo *CompPush
425         M_Out(12257) = 1 Dly 0.3    ' Y71 1:�ʒu����CY ����
426         fErrorProcess(11,231,282,0)
427     If M_20# = MNext% Then M_Out(12256) = 1 Dly 0.3 'Y70 1:�ʒu����CY �Œ�
428     If M_20# = MNext% Then M_20# = MClear%
429     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
430     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
431     If M_20# = MContinue% Then GoTo *RE_PUSH
432     *CompPush
433 '
434     *RE_READ
435     If M_20# = MContinue% Then M_20# = MClear%
436 '
437     MRtn = 1                            'MRtn������
438     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
439         MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
440     EndIf
441         '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
442         '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
443 '
444     If MRtn = 1 Then GoTo *CompRead
445     'fErrorProcess(11,97,25,0)
446 '    If M_20# = MPass% Then GoTo *ASSY_ERROR_END
447     If M_20# = MNext% Then M_20# = MClear%
448 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
449 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
450     If M_20# = MContinue% Then GoTo *RE_READ
451 '    If M_20# = MNext% Then M_20# = MPass%
452     GoTo *ASSY_ERROR_END
453     *CompRead
454     '
455 '�yMAIN���ID�ǂݍ��݁z
456     *RE_MEIN_CHECK
457     PInspPosition(1) = PMainPcbRead 'MAIN��Ǎ��ʒu
458     MInspGroup%(1) = 2              '����G�ԍ�
459     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
460 '
461     If MRtn = 1 Then GoTo *CompMainCheck
462     fErrorProcess(11,38,25,0)
463     If M_20# = MNext% Then M_20# = MClear%
464     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466     If M_20# = MContinue% Then GoTo *RE_MEIN_CHECK
467     *CompMainCheck
468 '�yGYRO���ID�ǂݍ��݁z
469     *RE_GYRO_CHECK
470     PInspPosition(1) = PGyroPcbRead 'GYRO��Ǎ��ʒu
471     MInspGroup%(1) = 3              '����G�ԍ�
472     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
473 '
474     If MRtn = 1 Then GoTo *CompGyroCheck
475     fErrorProcess(11,38,25,0)
476     If M_20# = MNext% Then M_20# = MClear%
477     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
478     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
479     If M_20# = MContinue% Then GoTo *RE_GYRO_CHECK
480     *CompGyroCheck
481 '�y���ID�R�s�[�z
482     *RE_PCB_RECORD
483     M_Out(12571) = 1    ' �̈�1 ��ԍ��R�s�[ (D2600-) On
484     Dly 0.1
485     M_Out(12572) = 1    ' �̈�2 ��ԍ��R�s�[ (D2612-) On
486     Dly 0.1
487     M_Out(12566) = 1    ' toPLC_��ԍ��R�s�[�v�� On
488 '
489     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��R�s�[���� On
490     If MRtn = 1 Then
491         M_Out(12571) = 0  ' �̈�1 ��ԍ��R�s�[ (D2600-) Off
492         Dly 0.1
493         M_Out(12572) = 0  ' �̈�2 ��ԍ��R�s�[ (D2612-) Off
494         Dly 0.1
495         M_Out(12566) = 0  ' toPLC_��ԍ��R�s�[�v�� Off
496 '        GoTo *RE_PCB_COMPAIRE   ' ��ԍ��ƍ��ɃX�L�b�v
497     Else
498         fErrorProcess(11,39,25,0)
499         If M_20# = MNext% Then M_20# = MClear%
500         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
501         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
502         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
503     EndIf
504 '�y���ID�ƍ��i�R�t���j�z
505     MRetryCount% = 0
506     While (MRetryCount% <= MRetryLimit%)
507         *RE_PCB_COMPAIRE
508         M_Out(12557)= 1 ' ��ԍ��ƍ��r�b�gON
509         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��ƍ�OK(M420) On
510         If MRtn = 1 Then
511             M_Out(12557)= 0     ' ��ԍ��ƍ��r�b�gOff
512             ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
513             MRetryCount% = 99
514         Else
515             If MRetryCount% = MRetryLimit% Then
516                 If M_In(11565) = 1 Then
517                     fErrorProcess(11,37,25,0)
518                 Else
519                     fErrorProcess(11,38,25,0)
520                 EndIf
521                 If M_20# = MNext% Then
522                     M_20# = MClear%
523                     ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
524                     MRetryCount% = 99
525                 EndIf
526                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
527                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
528                 If M_20# = MContinue% Then
529                     MRetryCount% = 0
530                 EndIf
531             Else
532                 ' ���g���C�񐔃C���N�������g
533                 MRetryCount% = MRetryCount% + 1
534                 Dly 0.1  ' ���̍H���ƃ^�C�~���O�����炷�ׂ̃f�B���C
535             EndIf
536         EndIf
537     WEnd
538 '
539     *RE_CHECK
540     PInspPosition(1) = PParts1Check '���i1�摜�`�F�b�N�ʒu(MAIN����Ӂj
541     MInspGroup%(1) = 4              '����G�ԍ�
542     PInspPosition(2) = PParts2Check '���i2�摜�`�F�b�N�ʒu�i�w�ʔ��Ӂj
543     MInspGroup%(2) = 5              '����G�ԍ�
544 '    PInspPosition(3) = PParts3Check '���i3�摜�`�F�b�N�ʒu�iSOC����Ӂj
545 '    MInspGroup%(3) = 6              '����G�ԍ�
546 '    PInspPosition(4) = PParts4Check '���i4�摜�`�F�b�N�ʒu�i�����Ӂj
547 '    MInspGroup%(4) = 7              '����G�ԍ�
548     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 2, -1, 1 )  '�摜�����������s
549     If MRtn = 1 Then GoTo *CompCheck
550     fErrorProcess(11,43,23,0)
551     If M_20# = MNext% Then M_20# = MClear%
552     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
553     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
554     If M_20# = MContinue% Then GoTo *RE_CHECK
555     *CompCheck
556     '
557     '���i�ʒu����(ID�Ǎ���ɕύX 9/16 M.Hayakawa�j
558     *RE_POS
559     If M_20# = MContinue% Then M_20# = MClear%
560     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
561     M_Out(12256)=1 Dly 0.3      '�ʒu����CY�pSV�o�[�p���X�o��
562     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
563 '
564     'Wait M_In(11266)=1          '�ʒu���ߏo�[���o�C���ɂ��R�����g�A�E�g(8/26����))
565     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
566     If MRtn = 1 Then GoTo *Comp_Pos_1
567     fErrorProcess(11,231,282,0)
568     If M_20# = MNext% Then M_20# = MClear%
569     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
570     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
571     If M_20# = MContinue% Then GoTo *RE_POS
572     *Comp_Pos_1
573     '
574     M_Out(12258)=1 Dly 0.3      '�v�b�V��CY�pSV�o�[�p���X�o��(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
575     M_Out(12260)=1 Dly 0.3      'FAN�N�����v�ߒ[�p���X�o��(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
576     Mov PScrewSupplyMain_1
577 '
578 '    M_Out(12258)=1 Dly 0.3      '�v�b�V��CY�pSV�o�[�p���X�o��
579     'Wait M_In(11268)=1          '�v�b�V���o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
580     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '�v�b�V���o�[���o
581     If MRtn = 1 Then GoTo *Comp_Pos_2
582     fErrorProcess(11,231,282,0)
583     If M_20# = MNext% Then M_20# = MClear%
584     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
585     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
586     If M_20# = MContinue% Then GoTo *RE_POS
587     *Comp_Pos_2
588     '
589 '    M_Out(12260)=1 Dly 0.3      'FAN�N�����v�ߒ[�p���X�o��(���C���˂����ߌ�ɕύX M.Hayakawa)(�^�N�g�Z�k�̂��߈ʒu�ړ�(12/13����))
590     'Wait M_In(11270)=1          'FAN�N�����v�ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
591     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'FAN�N�����v�ߒ[���o(8/26����)
592     If MRtn = 1 Then GoTo *Comp_Pos_3
593     fErrorProcess(11,231,282,0)
594     If M_20# = MNext% Then M_20# = MClear%
595     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
596     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
597     If M_20# = MContinue% Then GoTo *RE_POS
598     *Comp_Pos_3
599     '
600     '
601     'Main��̃l�W����
602     'Main��p�l�W�����@�փl�W�����ɍs��
603     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
604     '
605     '*ScrewSupplyMain           '�ꎞ�R�����g�A�E�g(�ȉ�5�s,8/5����)
606 '    Mov PScrewSupplyMain_2      '�l�W�����@���_
607 '    Mov PScrewSupplyMain_1      '�l�W�s�b�N�A�b�v���
608 '    Mvs PScrewSupplyMain        '�l�W�s�b�N�A�b�v
609 '    Mvs PScrewSupplyMain_1      '�l�W�s�b�N�A�b�v���
610 '    Mov PScrewSupplyMain_2      '�l�W�����@���_
611     'Return                     '�ꎞ�R�����g�A�E�g(8/4����)
612     'ScrewPositionDebug_1()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
613     '
614     PGetScrewPosTemp(1) = PScrewSupplyMain_1   '�l�W�s�b�N�A�b�v������(8/26����)
615     PGetScrewPosTemp(2) = PScrewSupplyMain_2   '�l�W�������_����(8/26����)
616     PGetScrewPosTemp(9) = PScrewSupplyMain_9   '�l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
617     PGetScrewPosTemp(10) = PScrewSupplyMain    '�l�W�s�b�N�A�b�v����(8/26����)
618     '
619     *RE_SCREW_GET_1                                '���g���C�p���x��
620     If M_20# = MContinue% Then M_20# = MClear%
621     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
622     If M_20# = MClear% Then GoTo *Comp_Screw_1
623     If M_20# = MNext% Then M_20# = MClear%
624     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
625     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
626     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
627     *Comp_Screw_1
628     '
629     '�@�ԃl�W����
630 '    Mov PScrewMain1_1           '�@���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
631 '    Ovrd 5
632 '    Mvs PScrewMain1             '�@�l�W����
633 '    Ovrd 10
634 '    Mvs PScrewMain1_1           '�@���
635     PScrewPosTemp(1) = PScrewMain1_1    '�l�W1���ߊJ�n�ʒu������(8/26����)
636     PScrewPosTemp(2) = PScrewMain1_0    '�l�W1���ߊJ�n�ʒu����(8/26����)
637     PScrewPosTemp(10) = PScrewMain1     '�l�W1���ߏI���ʒu����(8/26����)
638     M_Out16(12672) = 1              '�l�W���߈ʒu�ԍ����M
639     MRtn = ScrewTight(PScrewPosTemp,1,10.0)    '�l�W1���߂̎��s(8/26����)
640     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
641     If MRtn = 1 Then GoTo *CompScrew1
642     Mov PInitialPosition
643     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
644     MScrewErrorCord% = MScrewErrorCord% + 1
645     fErrorProcess(11,MScrewErrorCord%,52,0)
646 '    fErrorProcess(11,53,52,0)
647     If M_20# = MNext% Then M_20# = MClear%
648     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
649     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
650     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
651     *CompScrew1
652     '
653     'Main��p�l�W�����@�փl�W�����ɍs��
654     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
655     'ScrewPositionDebug_1()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
656     *RE_SCREW_GET_2                                '���g���C�p���x��
657     If M_20# = MContinue% Then M_20# = MClear%
658     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
659     If M_20# = MClear% Then GoTo *Comp_Screw_2
660     If M_20# = MNext% Then M_20# = MClear%
661     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
662     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
663     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
664     *Comp_Screw_2
665     '�A�ԃl�W����
666 '    Mov PScrewMain2_1           '�A���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
667 '    Ovrd 5
668 '    Mvs PScrewMain2             '�A�l�W����
669 '    Ovrd 10
670 '    Mvs PScrewMain2_1           '�A���
671     PScrewPosTemp(1) = PScrewMain2_1    '�l�W2���ߊJ�n�ʒu������(8/26����)
672     PScrewPosTemp(2) = PScrewMain2_0    '�l�W2���ߊJ�n�ʒu����(8/26����)
673     PScrewPosTemp(10) = PScrewMain2     '�l�W1���ߏI���ʒu����(8/26����)
674     M_Out16(12672) = 2              '�l�W���߈ʒu�ԍ����M
675     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����2�̎��s(8/26����)
676     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
677     If MRtn = 1 Then GoTo *CompScrew2
678     Mov PInitialPosition
679     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
680     MScrewErrorCord% = MScrewErrorCord% + 2
681     fErrorProcess(11,MScrewErrorCord%,52,0)
682 '    fErrorProcess(11,54,52,0)
683     If M_20# = MNext% Then M_20# = MClear%
684     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
685     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
686     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
687     *CompScrew2
688     '
689     'Main��p�l�W�����@�փl�W�����ɍs��
690     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
691     *RE_SCREW_GET_3                                '���g���C�p���x��
692     If M_20# = MContinue% Then M_20# = MClear%
693     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
694     If M_20# = MClear% Then GoTo *Comp_Screw_3
695     If M_20# = MNext% Then M_20# = MClear%
696     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
697     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
698     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
699     *Comp_Screw_3
700     '�B�ԃl�W����
701 '    Mov PScrewMain3_1           '�B���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
702 '    Ovrd 5
703 '    Mvs PScrewMain3             '�B�l�W����
704 '    Ovrd 10
705 '    Mvs PScrewMain3_1           '�B���
706     PScrewPosTemp(1) = PScrewMain3_1    '�l�W3���ߊJ�n�ʒu������(8/26����)
707     PScrewPosTemp(2) = PScrewMain3_0    '�l�W3���ߊJ�n�ʒu����(8/26����)
708     PScrewPosTemp(10) = PScrewMain3     '�l�W3���ߏI���ʒu����(8/26����)
709     M_Out16(12672) = 3              '�l�W���߈ʒu�ԍ����M
710     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����3�̎��s(8/26����)
711     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
712     If MRtn = 1 Then GoTo *CompScrew3
713     Mov PInitialPosition
714     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
715     MScrewErrorCord% = MScrewErrorCord% + 3
716     fErrorProcess(11,MScrewErrorCord%,52,0)
717 '    fErrorProcess(11,55,52,0)
718     If M_20# = MNext% Then M_20# = MClear%
719     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
720     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
721     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
722     *CompScrew3
723     '
724     'Main��p�l�W�����@�փl�W�����ɍs��
725     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
726     *RE_SCREW_GET_4                                '���g���C�p���x��
727     If M_20# = MContinue% Then M_20# = MClear%
728     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
729     If M_20# = MClear% Then GoTo *Comp_Screw_4
730     If M_20# = MNext% Then M_20# = MClear%
731     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
732     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
733     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
734     *Comp_Screw_4
735     '�C�ԃl�W����
736 '    Mov PScrewMain4_1           '�C���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
737 '    Ovrd 5
738 '    Mvs PScrewMain4             '�C�l�W����
739 '    Ovrd 10
740 '    Mvs PScrewMain4_1           '�C���
741     PScrewPosTemp(1) = PScrewMain4_1    '�l�W4���ߊJ�n�ʒu������(8/26����)
742     PScrewPosTemp(2) = PScrewMain4_0    '�l�W4���ߊJ�n�ʒu����(8/26����)
743     PScrewPosTemp(10) = PScrewMain4     '�l�W4���ߏI���ʒu����(8/26����)
744     M_Out16(12672) = 4              '�l�W���߈ʒu�ԍ����M
745     MRtn = ScrewTight(PScrewPosTemp,1,10.0)        '�l�W����4�̎��s(8/26����)
746     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
747     If MRtn = 1 Then GoTo *CompScrew4
748     Mov PInitialPosition
749     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
750     MScrewErrorCord% = MScrewErrorCord% + 4
751     fErrorProcess(11,MScrewErrorCord%,52,0)
752 '    fErrorProcess(11,56,52,0)
753     If M_20# = MNext% Then M_20# = MClear%
754     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
755     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
756     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
757     *CompScrew4
758     '
759     'Main��p�l�W�����@�փl�W�����ɍs��
760     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
761     *RE_SCREW_GET_5                                '���g���C�p���x��
762     If M_20# = MContinue% Then M_20# = MClear%
763     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
764     If M_20# = MClear% Then GoTo *Comp_Screw_5
765     If M_20# = MNext% Then M_20# = MClear%
766     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
767     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
768     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
769     *Comp_Screw_5
770     '�D�ԃl�W����
771 '    Mov PScrewMain5_1           '�D���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
772 '    Ovrd 5
773 '    Mvs PScrewMain5             '�D�l�W����
774 '    Ovrd 10
775 '    Mvs PScrewMain5_1           '�D���
776     PScrewPosTemp(1) = PScrewMain5_1    '�l�W5���ߊJ�n�ʒu������(8/26����)
777     PScrewPosTemp(2) = PScrewMain5_0    '�l�W5���ߊJ�n�ʒu����(8/26����)
778     PScrewPosTemp(10) = PScrewMain5     '�l�W5���ߏI���ʒu����(8/26����)
779     M_Out16(12672) = 5              '�l�W���߈ʒu�ԍ����M
780     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        '�l�W����5�̎��s(8/26����)
781     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
782     If MRtn = 1 Then GoTo *CompScrew5
783     Mov PInitialPosition
784     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
785     MScrewErrorCord% = MScrewErrorCord% + 5
786     fErrorProcess(11,MScrewErrorCord%,52,0)
787 '    fErrorProcess(11,57,52,0)
788     If M_20# = MNext% Then M_20# = MClear%
789     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
790     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
791     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
792     *CompScrew5
793     '
794     'Main��p�l�W�����@�փl�W�����ɍs��
795     'GoSub *ScrewSupplyMain     '�ꎞ�R�����g�A�E�g(8/4����)
796 '�ȉ�3�sPP�i�Ƀl�W�����Ȃ����߈ꎞ�폜 9/16 M.Hayakawa
797     *RE_SCREW_GET_6                                '���g���C�p���x��
798     If M_20# = MContinue% Then M_20# = MClear%
799     ScrewGet(PGetScrewPosTemp , 11259 , 11261)          '�l�W�󂯎��J�n
800     If M_20# = MClear% Then GoTo *Comp_Screw_6
801     If M_20# = MNext% Then M_20# = MClear%
802     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
803     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
804     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
805     *Comp_Screw_6
806     '�E�ԃl�W����
807 '    Mov PScrewMain6_1           '�E���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
808 '    Ovrd 5
809 '    Mvs PScrewMain6             '�E�l�W����
810 '    Ovrd 10
811 '    Mvs PScrewMain6_1           '�E���
812 '�ȉ�3�sPP�i�Ƀl�W�����Ȃ����߈ꎞ�폜 9/16 M.Hayakawa
813     PScrewPosTemp(1) = PScrewMain6_1    '�l�W6���ߊJ�n�ʒu������(8/26����)
814     PScrewPosTemp(2) = PScrewMain6_0    '�l�W6���ߊJ�n�ʒu����(8/26����)
815     PScrewPosTemp(10) = PScrewMain6     '�l�W6���ߏI���ʒu����(8/26����)
816     M_Out16(12672) = 6              '�l�W���߈ʒu�ԍ����M
817     MRtn = ScrewTight(PScrewPosTemp,6,10.0)        '�l�W����6�̎��s(8/26����)
818     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
819     If MRtn = 1 Then GoTo *CompScrew6
820     Mov PInitialPosition
821     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
822     MScrewErrorCord% = MScrewErrorCord% + 6
823     fErrorProcess(11,MScrewErrorCord%,52,0)
824 '    fErrorProcess(11,58,52,0)
825     If M_20# = MNext% Then M_20# = MClear%
826     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
827     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
828     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
829     *CompScrew6
830     '
831     'FAN�p�l�W�����@�փl�W�����ɍs��
832     'GoSub *ScrewSupplyFan      '�ꎞ�R�����g�A�E�g(8/4����)
833 ' �l�W�ʒu�w��O�Ɏ��ɂ����Ă���H 1�s�ꎞ�폜 9/16 M.Hayakawa
834 '    MRtn = ScrewGet(PGetScrewPosTemp)       '�l�W�����ɍs��(8/26����)
835     '
836 '    *ScrewSupplyFan
837 '    Mov PScrewSupplyFan_2       '�l�W�����@���_
838 '    Mov PScrewSupplyFan_1       '�l�W�s�b�N�A�b�v���
839 '    Mvs PScrewSupplyFan         ''�l�W�s�b�N�A�b�v
840 '    Mvs PScrewSupplyFan_1       '�l�W�s�b�N�A�b�v���
841 '    Mov PScrewSupplyFan_2       '�l�W�����@���_
842    ' Return                     '�ꎞ�R�����g�A�E�g(8/4����)
843     'ScrewPositionDebug_2()       '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
844     '
845     PGetScrewPosTemp(1) = PScrewSupplyFan_1   '�l�W�s�b�N�A�b�v������(8/26����)
846     PGetScrewPosTemp(2) = PScrewSupplyFan_2   '�l�W�������_����(8/26����)
847     PGetScrewPosTemp(9) = PScrewSupplyFan_9   '�l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
848     PGetScrewPosTemp(10) = PScrewSupplyFan    '�l�W�s�b�N�A�b�v����(8/26����)
849 '
850     *RE_SCREW_GET_7                                '���g���C�p���x��
851 '
852     If M_20# = MContinue% Then M_20# = MClear%
853     ScrewGet(PGetScrewPosTemp , 11260 , 0)          '�l�W�󂯎��J�n
854     If M_20# = MClear% Then GoTo *Comp_Screw_7
855     If M_20# = MNext% Then M_20# = MClear%
856     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
857     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
858     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
859     *Comp_Screw_7
860     '�F�ԃl�W����
861 '    Mov PScrewFan1_1            '�F���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
862 '    Ovrd 5
863 '    Mvs PScrewFan1              '�F�l�W����
864 '    Ovrd 10
865 '    Mvs PScrewFan1_1            '�F���
866     PScrewPosTemp(1) = PScrewFan1_1    'Fan1�l�W���ߊJ�n�ʒu������(8/26����)
867     PScrewPosTemp(2) = PScrewFan1_0    'Fan1�l�W���ߊJ�n�ʒu����(8/26����)
868     PScrewPosTemp(10) = PScrewFan1     'Fan1�l�W���ߏI���ʒu����(8/26����)
869     M_Out16(12672) = 7              '�l�W���߈ʒu�ԍ����M
870     MRtn = ScrewTight(PScrewPosTemp,2,6.7)       'Fan�l�W����1�̎��s(8/26����)
871     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
872     If MRtn = 1 Then GoTo *CompScrew7
873     Mov PInitialPosition
874     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
875     MScrewErrorCord% = MScrewErrorCord% + 7
876     fErrorProcess(11,MScrewErrorCord%,52,0)
877 '    fErrorProcess(11,59,52,0)
878     If M_20# = MNext% Then M_20# = MClear%
879     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
880     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
881     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_7
882     *CompScrew7
883 '
884     '
885     'FAN�p�l�W�����@�փl�W�����ɍs��
886     'GoSub *ScrewSupplyFan      '�ꎞ�R�����g�A�E�g(8/4����)
887     'ScrewPositionDebug_2()      '�f�o�b�N�p(�ʊ֐��g�p�̂��߃R�����g�A�E�g(8/26����))
888     *RE_SCREW_GET_8                                '���g���C�p���x��
889     If M_20# = MContinue% Then M_20# = MClear%
890     ScrewGet(PGetScrewPosTemp , 11260 , 0)          '�l�W�󂯎��J�n
891     If M_20# = MClear% Then GoTo *Comp_Screw_8
892     If M_20# = MNext% Then M_20# = MClear%
893     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
894     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
895     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
896     *Comp_Screw_8
897     '�G�ԃl�W����
898 '    Mov PScrewFan2_1            '�G���(�ȉ�5�s�ꎞ�R�����g�A�E�g(8/26����))
899 '    Ovrd 5
900 '    Mvs PScrewFan2              '�G�l�W����
901 '    Ovrd 10
902 '    Mvs PScrewFan2_1            '�G���
903     PScrewPosTemp(1) = PScrewFan2_1    'Fan2�l�W���ߊJ�n�ʒu������(8/26����)
904     PScrewPosTemp(2) = PScrewFan2_0    'Fan2�l�W���ߊJ�n�ʒu����(8/26����)
905     PScrewPosTemp(10) = PScrewFan2     'Fan2�l�W���ߏI���ʒu����(8/26����)
906     M_Out16(12672) = 8              '�l�W���߈ʒu�ԍ����M
907     MRtn = ScrewTight(PScrewPosTemp,2,6.7)       'Fan�l�W����2�̎��s(8/26����)
908     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
909     If MRtn = 1 Then GoTo *CompScrew8
910     Mov PInitialPosition
911     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
912     MScrewErrorCord% = MScrewErrorCord% + 8
913     fErrorProcess(11,MScrewErrorCord%,52,0)
914 '    fErrorProcess(11,60,52,0)
915     If M_20# = MNext% Then M_20# = MClear%
916     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
917     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
918     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_8
919     *CompScrew8
920 '
921     '�v���O�������_
922     'Mov PInitialPosition        ' ���_���
923     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
924     Mov PTicketRead_1           ' �`�P�b�g���[�h�ʒu
925     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
926     InitialState()              ' ������Ԃɂ���
927     M_20# = MAssyOK%              ' ����I������
928     GoTo *fnAssyStart_FEndPosi
929 '
930 *ASSY_ERROR_END
931     fnInitialZone()   ' �����ʒu�Ɉړ�
932     InitialState()  ' ������Ԃɂ���
933 *AssyEnd
934 *fnAssyStart_FEndPosi
935     Exit Function
936 FEnd
937 '
938 '��fnPiasCheck
939 ''' <summary>
940 ''' PIAS�`�P�b�g�Ǎ���
941 ''' </summary>
942 ''' <returns>   0 : NG
943 '''             1 : OK(�Ǎ��݊���)
944 ''' </returns>
945 ''' <remarks>
946 ''' Date   : 2021/07/07 : M.Hayakawa
947 ''' </remarks>'
948 ''' <Update>
949 ''' Date   : 2022/01/11 : ����
950 ''' </Update>
951 Function M% fnPiasCheck
952     fnPiasCheck = 0
953     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
954     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����
955 '
956 *RETRY_PIAS
957     M_20# = MClear%
958     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
959     '
960     '�yID�`�P�b�g�ǂݍ��݁z
961     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
962     MInspGroup%(1) = 1              '����G�ԍ�
963     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
964 '
965     '�G���[�̏ꍇ
966     If MRtn <> 1 Then
967         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
968         If MRtn <> 1 Then
969             'D720 -> D1300 �R�s�[�v��
970             M_Out(12565) = 1
971             Dly 0.5
972             M_Out(12565) = 0
973             '�G���[�����L�q
974             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
975             'GOT KEY���͑҂�
976             MKeyNumber = fnKEY_WAIT()
977             '
978             Select MKeyNumber
979                 Case MNext%         '���ւ�I�������ꍇ
980                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
981                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
982                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
983                     Break
984                 Case MAbout%        '��~��I�������ꍇ
985                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
986                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
987                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
988                     Break
989                 Case MNgProcess%    'NG��I�������ꍇ
990                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
991                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
992                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
993                     Break
994                 Case MContinue%     '�p����I�������ꍇ
995                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
996                     M_20# = MContinue%
997                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
998                     Break
999             End Select
1000         EndIf
1001     EndIf
1002     If M_20# = MPass% Then GoTo *fnPiasCheck_End            'PIAS�`�F�b�N�I��
1003     If M_20# = MAbout% Then GoTo *fnPiasCheck_End           'PIAS�`�F�b�N�I��
1004     If M_20# = MNgProcess% Then GoTo *fnPiasCheck_End       'PIAS�`�F�b�N�I��
1005     If M_20# = MContinue% Then GoTo *RETRY_PIAS             'PIAS�`�F�b�N���g���C
1006 '----------D720 -> D1300 �R�s�[�v��----------
1007     M_Out(12565) = 1
1008     Dly 0.5
1009     M_Out(12565) = 0
1010 '----------�ʐM�m�F������----------
1011     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1012     MRtn = 0                ' ������
1013     M_20# = MClear%         ' ������
1014     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1015     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1016 '    If MRtn <> 1 Then
1017 '        If M_20# = MContinue% Then
1018 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1019 '        Else
1020 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1021 '        EndIf
1022 '    EndIf
1023     If MRtn = 1 Then GoTo *PCComu_OK                '�ʐMOK�����x���փW�����v
1024     If M_20# = MContinue% Then GoTo *RETRY_PIAS      '�`�P�b�g�ǂݒ������烊�g���C
1025     GoTo *fnPiasCheck_End                           '���̑��̏ꍇPIAS�`�F�b�N�I��
1026     *PCComu_OK
1027 '----------�H�������m�F----------
1028     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1029     MRtn = 0                ' ������
1030     M_20# = MClear%         ' ������
1031     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1032     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1033 '    If MRtn <> 1 Then
1034 '        If M_20# = MContinue% Then
1035 '            GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1036 '        Else
1037 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1038 '        EndIf
1039 '    EndIf
1040     If MRtn = 1 Then GoTo *ProcessCheck_OK                '�H���`�F�b�NOK�����x���փW�����v
1041     If M_20# = MContinue% Then GoTo *RETRY_PIAS      '�`�P�b�g�ǂݒ������烊�g���C
1042     GoTo *fnPiasCheck_End                           '���̑��̏ꍇPIAS�`�F�b�N�I��
1043     *ProcessCheck_OK
1044     '
1045     fnPiasCheck = 1
1046     *fnPiasCheck_End
1047     Exit Function
1048 FEnd
1049 '
1050 '��fnPCComuCheck
1051 ''' <summary>
1052 ''' PC-PLC�ʐM�`�F�b�N
1053 ''' </summary>
1054 ''' <returns>   0 : NG
1055 '''             1 : OK(�Ǎ��݊���)
1056 ''' </returns>
1057 ''' <remarks>
1058 ''' Date   : 2021/07/07 : M.Hayakawa
1059 ''' </remarks>'
1060 Function M% fnPCComuCheck
1061     fnPCComuCheck = 0
1062     MJudge% = 0                                  '������
1063     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1064     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1065     '
1066     For MStaNo = 0 To 5
1067         '
1068         If M_In(MIN_PIAS_ComOK%) = 1 Then
1069             'PC�ʐMOK(M400)
1070             MJudge% = MOK%
1071             MStaNo = 5
1072             Break
1073         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1074             'toRBT_�ʐM�m�Ftime out
1075             MJudge% = MNG%
1076             MCommentD1001 = 15
1077             MCommentD1002 = 21
1078             MStaNo = 5
1079             Break
1080         Else
1081             'toRBT_�ʐM�m�Ftime out
1082             MJudge% = MNG%
1083             MCommentD1001 = 14
1084             MCommentD1002 = 21
1085             Break
1086         EndIf
1087     Next MStaNo
1088     '
1089     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1090     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1091     '
1092     '�G���[���
1093     If MJudge% <> MOK% Then
1094         M_20# = MClear%     '������
1095         '�G���[�����L�q
1096         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1097         'GOT KEY���͑҂�
1098         MKeyNumber = fnKEY_WAIT()
1099         '
1100         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1101             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1102             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1103             Break
1104         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1105             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1106             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1107             Break
1108         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1109             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1110             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1111             Break
1112         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1113             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1114             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1115             Break
1116         EndIf
1117     Else
1118         'OK�̏ꍇ
1119         fnPCComuCheck = 1
1120     EndIf
1121     Exit Function
1122 FEnd
1123 '
1124 '��fnProcessCheck
1125 ''' <summary>
1126 ''' �H�������m�F
1127 ''' </summary>
1128 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1129 '''             -1�F�O�H������NG  -2�F���H����������
1130 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1131 '''             -5�F���������G���[
1132 ''' </returns>
1133 ''' <remarks>
1134 ''' Date   : 2021/07/07 : M.Hayakawa
1135 ''' </remarks>'
1136 Function M% fnProcessCheck
1137     fnProcessCheck = 0
1138     MJudge% = MNG%      '��UNG���������Ƃ���
1139 '----------�H�������m�F----------
1140     MCommentD1001 = 0   '�R�����g������
1141     For MStaNo = 0 To 5
1142         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1143         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1144         '
1145         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1146             MJudge% = MOK%
1147             fnAutoScreenComment(85)     ' AUTO���
1148             MStaNo = 5
1149             Break
1150         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1151             MFlgLoop% = 0
1152             MJudge% = MNG%
1153             MCommentD1001 = 27
1154             MCommentD1002 = 22
1155             fnAutoScreenComment(94)     ' AUTO���
1156             fnProcessCheck = -2         ' NG��-2��Ԃ�
1157             MStaNo = 5
1158             Break
1159         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1160            MJudge% = MNG%
1161             MCommentD1001 = 31
1162             MCommentD1002 = 22
1163             fnAutoScreenComment(83)     ' AUTO���
1164             fnProcessCheck = -3         ' NG��-3��Ԃ�
1165             MStaNo = 5
1166             Break
1167         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1168             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1169             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1170             MJudge% = MNG%
1171             MCommentD1001 = 32
1172             MCommentD1002 = 22
1173             fnAutoScreenComment(84)     ' AUTO���
1174             fnProcessCheck = -1         ' NG��-1��Ԃ�
1175             Dly 1.0
1176             '�H�������m�FOFF
1177             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1178             Dly 1.0
1179            'MStaNo = 5
1180             Break
1181         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1182             MFlgLoop% = 0
1183             MJudge% = MNG%
1184             MCommentD1001 = 29
1185             MCommentD1002 = 22
1186             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1187             fnProcessCheck = -5         ' NG��-5��Ԃ�
1188             MStaNo = 5
1189             Break
1190         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1191             MJudge% = MNG%
1192             If MCommentD1001 = 32 Then
1193                 '�������Ȃ�
1194             Else
1195                 MCommentD1001 = 26
1196             EndIf
1197             MCommentD1002 = 22
1198             fnProcessCheck = -4         ' NG��-4��Ԃ�
1199             MStaNo = 5
1200             Break
1201         Else
1202             MJudge% = MNG%
1203             MCommentD1001 = 28
1204             MCommentD1002 = 22
1205         EndIf
1206     Next MStaNo
1207     '�H�������m�FOFF
1208     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1209     '�ʉߗ���NG �H�������̏ꍇ
1210     If MJudge% = MPass% Then
1211         M_20# = MPass%
1212     EndIf
1213     '
1214     '�G���[���
1215     If MJudge% <> MOK% Then
1216         M_20# = MClear%     '������
1217         '�G���[�����L�q
1218         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1219         'GOT KEY���͑҂�
1220         MKeyNumber = fnKEY_WAIT()
1221         '
1222         Select MKeyNumber
1223             Case MAbout%        '��~��I�������ꍇ
1224                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1225                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1226                 Break
1227             Case MNext%         '���ւ�I�������ꍇ
1228                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1229                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1230                 Break
1231             Case MContinue%     '�p����I�������ꍇ
1232                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1233                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1234                 Break
1235             Case MNgProcess%    'NG��I�������ꍇ
1236                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1237                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1238                 Break
1239         End Select
1240     Else
1241         fnProcessCheck = 1  ' OK��1��Ԃ�
1242     EndIf
1243     Exit Function
1244 FEnd
1245 '
1246 '��fnPiasWrite
1247 ''' <summary>
1248 ''' Pias �g�����ʏ����ݗv��
1249 ''' </summary>
1250 '''<param name="MFlg%">
1251 ''' MOK%(1) = �H��������OK��������
1252 ''' MNG%(0) = �H��������NG��������
1253 '''</param>
1254 '''<returns></returns>
1255 ''' <remarks>
1256 ''' Date   : 2021/07/07 : M.Hayakawa
1257 ''' </remarks>'
1258 Function M% fnPiasWrite(ByVal MFlg%)
1259       fnPiasWrite = 0
1260 *RETRY_PIASWRITE
1261     '
1262     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1263    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1264     If MFlg% = MOK% Then
1265         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1266     Else
1267         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1268     EndIf
1269     Dly 0.1                  '�O�̂���
1270     '
1271     'Pias�֏����݊J�n M305 -> ON
1272     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1273     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1274     '
1275     MJudge% = MNG%
1276     '
1277     For MStaNo = 0 To 5
1278         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1279             MJudge% = MOK%
1280             'MRet = fnAutoScreenComment(85)  'AUTO���
1281             MStaNo = 5
1282             Break
1283         '
1284         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1285             MJudge% = MNG%
1286             'MRet = fnAutoScreenComment(85)  'AUTO���
1287            MCommentD1001 = 34
1288            MCommentD1002 = 25
1289             MStaNo = 5
1290             Break
1291         '
1292         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1293             MJudge% = MNG%
1294             'MRet = fnAutoScreenComment(85)  'AUTO���
1295            MCommentD1001 = 35
1296            MCommentD1002 = 25
1297             MStaNo = 5
1298             Break
1299         '
1300         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1301             MJudge% = MNG%
1302             'MRet = fnAutoScreenComment(85)  'AUTO���
1303            MCommentD1001 = 36
1304            MCommentD1002 = 25
1305             MStaNo = 5
1306             Break
1307         '
1308         Else
1309             MJudge% = MNG%
1310            MCommentD1001 = 42
1311            MCommentD1002 = 25
1312         '
1313         EndIf
1314         '
1315     Next MStaNo
1316     '
1317     'Pias�֏����݊J�n M305 -> OfF
1318     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1319     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1320     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1321     '
1322     '
1323     '�ʉߗ���NG �H�������̏ꍇ
1324     If MJudge% = MPass% Then
1325         M_20# = MPass%
1326     EndIf
1327     '
1328    M_20# = MClear%     '������
1329     '
1330     '�G���[���
1331     If MJudge% < MOK% Then
1332     '
1333 '�c���Ă���������ł͎g�p���Ȃ����x��
1334 *RETRY_ERR_WRITE
1335         M_20# = MClear%     '������
1336         '�G���[�����L�q
1337         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1338         'GOT KEY���͑҂�
1339         MKeyNumber = fnKEY_WAIT()
1340         '
1341         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1342             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1343            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1344             Break
1345         '
1346         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1347             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1348             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1349             Break
1350         '
1351         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1352             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1353             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1354             Break
1355         '
1356         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1357             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1358            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1359             Break
1360         '
1361         EndIf
1362         '
1363 '        If M_20# = MClear% Then *RETRY_ERR_WRITE
1364         '
1365     EndIf
1366     '
1367     If M_20# = MContinue% Then *RETRY_PIASWRITE
1368     '
1369     fnPiasWrite = 1
1370     Exit Function
1371 FEnd
1372 '
1373 '��fnPCBNumberCheck
1374 ''' <summary>
1375 ''' Pias ��ԍ��ƍ��v��
1376 ''' </summary>
1377 '''<returns>0�i�Œ�j</returns>
1378 ''' <remarks>
1379 ''' Date   : 2021/07/07 : M.Hayakawa
1380 ''' </remarks>'
1381 Function M% fnPCBNumberCheck
1382       fnPCBNumberCheck = 0
1383     '
1384 *RETRY_PCBCHECK
1385     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1386     'Pias�֊�ƍ��J�n M310 -> ON
1387     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1388     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1389     '
1390     MJudge% = MNG%
1391     '
1392     For MStaNo = 0 To 5
1393         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1394             MJudge% = MOK%
1395             fnAutoScreenComment(96)  'AUTO���
1396             MStaNo = 5
1397             Break
1398         '
1399         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1400             MJudge% = MNG%
1401             fnAutoScreenComment(97)  'AUTO���
1402             MCommentD1001 = 37
1403             MCommentD1002 = 25
1404             MStaNo = 5
1405             Break
1406         '
1407         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1408             MJudge% = MNG%
1409             fnAutoScreenComment(98)  'AUTO���
1410             MCommentD1001 = 38
1411             MCommentD1002 = 25
1412             MStaNo = 5
1413             Break
1414         '
1415         ElseIf M_In(11580) = 1 Then                         'time out
1416             MJudge% = MNG%
1417             fnAutoScreenComment(99)  'AUTO���
1418             MCommentD1001 = 39
1419             MCommentD1002 = 25
1420             MStaNo = 5
1421             Break
1422         '
1423         Else
1424             MJudge% = MNG%
1425            MCommentD1001 = 41
1426            MCommentD1002 = 25
1427         '
1428         EndIf
1429         '
1430     Next MStaNo
1431     '
1432     'Pias�֊�ƍ��J�n M310 -> OfF
1433     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1434     '
1435     '
1436     '�ʉߗ���NG �H�������̏ꍇ
1437     If MJudge% = MPass% Then
1438         M_20# = MPass%
1439     EndIf
1440     '
1441    M_20# = MClear%     '������
1442     '
1443     '�G���[���
1444     If MJudge% < MOK% Then
1445     '
1446 '�c���Ă���������ł͎g�p���Ȃ����x��
1447 *RETRY_ERR_PCBNUMBER
1448         M_20# = MClear%     '������
1449         '�G���[�����L�q
1450         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1451         'GOT KEY���͑҂�
1452         MKeyNumber = fnKEY_WAIT()
1453         '
1454         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1455             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1456             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1457             Break
1458         '
1459         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1460             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1461             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1462         '
1463         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1464             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1465             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1466         '
1467         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1468             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1469             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1470             Break
1471         '
1472         EndIf
1473         '
1474 '        If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1475         '
1476     EndIf
1477     '
1478     If M_20# = MContinue% Then *RETRY_PCBCHECK
1479     Exit Function
1480 FEnd
1481 '
1482 '��ScrewTight
1483 ''' <summary>
1484 ''' �˂����߂��s��(S�^�C�g)
1485 ''' </summary>
1486 '''<param name="PScrewPos()">
1487 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1488 '''             PScrewPos(2)    �F�˂����߉��_
1489 '''             PScrewPos(10)   �F�˂����ߏI������
1490 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
1491 '''             1:6mm S�^�C�g��l�W
1492 '''             2:8mm P�^�C�g
1493 '''             3:6mm S�^�C�g���l�W
1494 '''             4:13mm S�^�C�g
1495 '''             5:6mm M�l�W
1496 '''</param>
1497 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
1498 '''<returns>����
1499 '''         0=�ُ�I���A1=����I��
1500 '''</returns>
1501 ''' <remarks>
1502 ''' Date   : 2021/07/07 : M.Hayakawa
1503 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
1504 ''' </remarks>'
1505 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
1506     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1507     ScrewTight = 0
1508     MOKNGFlg = 0
1509     Ovrd 100
1510     Mov PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
1511     Fine 0.05 , P
1512     Ovrd MOvrdA%
1513     ' �����ݒ�
1514     Accel 100, 10
1515     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
1516     Mvs PScrewPosition(2)
1517     ' �����������ɖ߂�
1518     Accel
1519     ' ����Ovrd�ݒ�
1520 '    Ovrd MOvrdA%
1521     Ovrd 100
1522     ' Spd�ݒ�
1523 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1524     Spd MFeedSpd
1525     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
1526     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1527     Select MScrewType%
1528         Case 1
1529             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1530             ProgramBankSet(1,1)
1531             Break
1532         Case 2
1533             ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1534             ProgramBankSet(3,1)
1535             Break
1536         Case 3
1537             ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1538             ProgramBankSet(1,1)
1539             Break
1540         Case 4
1541             ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1542             ProgramBankSet(1,1)
1543             Break
1544         Case 5
1545             ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1546             ProgramBankSet(1,1)
1547             Break
1548         Case 6
1549             ' S�^�C�g�F�v���O����1�A�o���N4�ɐݒ�
1550             ProgramBankSet(1,4)
1551             Break
1552         Default
1553             ' �v���O����1�A�o���N�Ȃ��ݒ�
1554             ProgramBankSet(0,0)
1555             Break
1556     End Select
1557 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1558      '�h���C�o�[ON�@CW
1559     M_Out(12241)=1
1560     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1561     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
1562     Dly 0.1
1563     Fine 0 , P
1564     Spd M_NSpd
1565     '
1566     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
1567         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1568         Dly 0.1
1569        ' �v���O�����E�o���N����
1570         ProgramBankSet(0,0)
1571         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1572         Mvs PScrewPosition(10),-80
1573         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1574         M_Out(12249)=1 Dly 0.3
1575         MOKNGFlg = -1
1576         ScrewTight = 0
1577     Else
1578          '�h���C�o�[OFF�@CW
1579         M_Out(12241)=0
1580 '        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
1581 '        Select MScrewType%
1582 '            Case 1
1583 '                ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1584 '                ProgramBankSet(1,3)
1585 '                Break
1586 '            Case 2
1587 '                ' P�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1588 '                ProgramBankSet(3,3)
1589 '                Break
1590 '            Case 3
1591 '                ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
1592 '                ProgramBankSet(1,3)
1593 '                Break
1594 '            Case 4
1595 '                ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
1596 '                ProgramBankSet(1,3)
1597 '                Break
1598 '            Case 5
1599 '                ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
1600 '                ProgramBankSet(1,3)
1601 '                Break
1602 '            Default
1603 '                ' �v���O����1�A�o���N�Ȃ��ݒ�
1604 '                ProgramBankSet(0,0)
1605 '                Break
1606 '        End Select
1607 '         '�h���C�o�[ON�@CW
1608 '        Mvs PScrewPosition(10)
1609 '        M_Out(12241)=1
1610 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1611 '
1612          '�h���C�o�[OFF�@CW
1613         M_Out(12241)=0
1614        ' �v���O�����E�o���N����
1615         ProgramBankSet(0,0)
1616         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1617         M_Out(12249)=1 Dly 0.3
1618     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1619         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1620        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1621         'Mvs PScrewPosition(10),-80
1622         ScrewTight = 1
1623     EndIf
1624 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1625 '    Ovrd 10
1626 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1627     Ovrd 100
1628     Exit Function
1629 FEnd
1630 '
1631 '��ScrewGet
1632 ''' <summary>
1633 ''' �˂������@����˂��𓾂�
1634 ''' </summary>
1635 '''<param name="%">
1636 '''         PScrewPos(1)    �F�˂�������̂˂����
1637 '''         PScrewPos(2)    �F�˂���������_
1638 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1639 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1640 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1641 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1642 '''</param>
1643 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1644 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1645 '''<returns>����
1646 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1647 '''</returns>
1648 ''' <remarks>
1649 ''' Date   : 2021/07/07 : M.Hayakawa
1650 ''' </remarks>
1651 '''<update>
1652 '''Date    : 2021/11/15 : ����
1653 '''Date    : 2021/02/07 : ���� �O�̂��ߊm�F���폜
1654 '''</update>
1655 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1656     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
1657     ScrewGet = 0
1658     MScrewJudge% = 0
1659     MFinCnt% = 2
1660     '�˂������평������G���[�`�F�b�N
1661 ' ���b��폜
1662     'Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
1663     For MCnt% = 0 To MFinCnt%
1664         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1665         If MRtn = 0 Then
1666             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1667             ScrewGet = -1
1668             MScrewJudge% = 2
1669         EndIf
1670         Ovrd 100
1671         If FeederScrewSensor% <> 0 Then
1672             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1673                 'Ovrd 30
1674                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1675                 'NG�Ƃ��Ă����̊֐����甲����
1676                 ScrewGet = -2
1677                 MScrewJudge% = 3
1678             EndIf
1679         EndIf
1680         Ovrd 100
1681         Spd M_NSpd
1682         If MScrewJudge% = 0 Then
1683     '        ScrewGet = 0
1684             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1685 '            Dly 0.3
1686             MScrewCnt% = 0
1687             MFinCnt% = 2
1688             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1689             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1690             '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX4/3����)
1691             M_Out(Y60_Driver)=1
1692             '�r�b�g��]�����ԊĎ��J�n
1693             '
1694             '
1695             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1696             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1697             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1698             'Mvs PScrewPosition(10), 1.2
1699            Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
1700 '            '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX4/3����)
1701 '            M_Out(Y60_Driver)=1
1702 '            '�r�b�g��]�����ԊĎ��J�n
1703             M_Timer(4) = 0
1704             MloopFlg = 0
1705             MCrtTime& = 0
1706            '�r�b�g��]����܂őҋ@
1707             While MloopFlg = 0
1708                 MCrtTime& = M_Timer(4)
1709                 If MCrtTime& >= 180 Then
1710                     MloopFlg = 1
1711                 EndIf
1712             WEnd
1713             '
1714            M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1715             '�r�b�g��](�V�[�P���X�������ɂ������ʒu�ύX4/3����)
1716 '            M_Out(Y60_Driver)=1
1717 '            Dly 0.2
1718             '�z���ʒu�ɂċz���m�F
1719             MRtn = 0
1720             MRtn = frInCheck(11264, 1, MSETTIMEOUT01&)
1721             '
1722             JOvrd M_NJovrd
1723             Spd M_NSpd
1724             '�l�W�z���m�F�ʒu�ړ�
1725             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1726             Mvs PScrewPosition(10), -30  ' �l�W�z���m�F�ʒu
1727            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1728             '�r�b�g��]��~
1729             M_Out(Y60_Driver)=0
1730             '
1731 '            If MRtn = 1 Then            '�V�[�P���X�ύX�ɂ��R�����g�A�E�g(5/13����)
1732                 '1�b�ԃl�W�z���m�F �n�߂�臒l
1733                 MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1734 '            EndIf                       '�V�[�P���X�ύX�ɂ��R�����g�A�E�g(5/13����)
1735             'MRtn = 0'�����G���[
1736             '�z���G���[�̏ꍇ
1737             '�l�W���˂����Y�ɖ߂�
1738             If MRtn = 0 Then
1739                 Ovrd 5      '2����5�ɕύX
1740                 '�r�b�g��]��~
1741                 M_Out(Y60_Driver)=0
1742                 '�l�W�����@���
1743                 Mvs PScrewPosition(1)
1744                 '�X�ɏ��
1745                 Mov PScrewPosition(1), -140
1746                 '�l�W�̂Ĉʒu
1747                 If FeederReadyNo% = 11260 Then     '�����@�ʂɋz���G���[�����J�E���g�@2022/05/19 �n��
1748                     MRtn = FnCtlValue2(3)          '�����@�Q�z���G���[���{�P
1749                 Else
1750                     MRtn = FnCtlValue2(4)          '�����@�P�z���G���[���{�P  2022/04/28 �n��
1751                 EndIf
1752                 Mov PScrewPosition(9)
1753                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1754                 '�z��OFF
1755                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1756                 Dly 0.2
1757                 '�j��ON
1758                 M_Out(Y6B_VB1)=1 '�^��j��ON
1759                 '�r�b�g��]
1760                 M_Out(Y61_Driver)=1
1761                 Dly 0.5
1762                 '                '
1763                 Ovrd 100
1764                 JOvrd M_NJovrd
1765                 Spd M_NSpd
1766                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1767                 Mov PScrewPosition(9), 10
1768                 Mov PScrewPosition(9)
1769                 Dly 0.1
1770                 Mov PScrewPosition(9), 10
1771                 Mov PScrewPosition(9)
1772                 '
1773                 '�l�W�����҂�
1774                 Wait M_In(11265) = 0
1775                 '�r�b�g��]��~
1776                 M_Out(Y61_Driver)=0
1777                 Dly 0.1
1778                 '�j��OFF
1779                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1780                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1781                 Mov PScrewPosition(1), -140
1782                 Ovrd 100
1783                 Spd M_NSpd
1784                 '�l�W�����@���
1785                 Mvs PScrewPosition(1)
1786 '                '
1787                 ScrewGet = -3
1788                 If MCnt% = MFinCnt% Then
1789                     MScrewJudge% = 4
1790                     Mov PScrewPosition(2)
1791                     Break
1792                 EndIf
1793                 Break
1794 '                '
1795             Else
1796                 MCnt% = MFinCnt%
1797                 ScrewGet = 1
1798             EndIf
1799         Else
1800             MCnt% =MFinCnt%
1801         EndIf
1802     Next  MCnt%
1803         '
1804 '    If MScrewJudge% = 0 Then
1805 '        Ovrd 100
1806 '        Spd M_NSpd
1807 '        PScrewPosition(1)
1808 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1809 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1810 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1811 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1812 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1813 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1814 '        'Mov PScrewPosition(2)
1815 '        '������x�z���m�F�@���̍ŏI臒l
1816 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1817 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1818 '            MScrewJudge% = 4
1819 '            ScrewGet = -3
1820 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1821 '            MScrewJudge% = 1
1822 '            ScrewGet = 1
1823 '        EndIf
1824 '        Break
1825 '    EndIf
1826     '
1827 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1828     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1829     '
1830     Select MScrewJudge%
1831 '        Case 0
1832 ''            fErrorProcess(11,162,163,0) '�ُ�I��
1833 '            MCommentD1001 = 162
1834 '            MCommentD1002 = 96
1835 '            Break
1836         Case 2
1837 '            fErrorProcess(11,63,161,0) '����NG
1838             MCommentD1001 = 63
1839             MCommentD1002 = 96
1840             Break
1841         Case 3
1842 '            fErrorProcess(11,160,164,0) '�닟��
1843             MCommentD1001 = 237
1844             MCommentD1002 = 96
1845             Break
1846         Case 4
1847 '            fErrorProcess(11,94,95,0) '�z��NG
1848             MCommentD1001 = 94
1849             MCommentD1002 = 95
1850             Break
1851     End Select
1852     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1853     '
1854     Select M_20#
1855         Case MAbout%          '��~�������ꂽ�ꍇ
1856             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
1857             Mov PInitialPosition
1858             Break
1859         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1860             Break
1861         Case MNext%           '�p���������ꂽ�ꍇ
1862             M_20# = MClear%     '������
1863             Break
1864         Case MNgProcess%      'NG�������ꂽ�ꍇ
1865             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
1866             Mov PInitialPosition
1867             Break
1868         End Select
1869 *End_ScrewGet
1870     Exit Function
1871 FEnd
1872 '
1873 '��ProgramBankSet
1874 ''' <summary>
1875 ''' �˂����߂��s��(P�^�C�g)
1876 ''' </summary>
1877 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1878 '''<param name="MBankNo">�o���N�ԍ�</param>
1879 '''</returns>
1880 ''' <remarks>
1881 ''' Date   : 2021/10/05 : M.Hayakawa
1882 ''' </remarks>'
1883 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1884 '
1885     MLocalPrgNo% = (MProgramNo% - 1) * 32
1886     MLocalBankNo% = MBankNo% * 4
1887 '
1888     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1889         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1890     Else
1891         MLocalOutNo% = 0
1892     EndIf
1893 '
1894     M_Out8(12240) = MLocalOutNo%
1895     Dly 0.1
1896     Exit Function
1897 FEnd
1898 '
1899 '��fnKEY_WAIT()
1900 ''' <summary>
1901 ''' GOT����̃L�[���͑҂�
1902 ''' </summary>
1903 '''<returns>1�F��~    2�F����
1904 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1905 '''         5�FNG
1906 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1907 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1908 '''</returns>
1909 ''' <remarks>
1910 ''' Date   : 2021/07/07 : M.Hayakawa
1911 ''' </remarks>'
1912 Function M% fnKEY_WAIT()
1913     fnKEY_WAIT = 0
1914     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1915     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1916     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1917     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1918     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1919     Dly 0.2
1920     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1921     MLocalLoopFlg=1
1922     While MLocalLoopFlg=1
1923         If M_In(11345) = 1 Then         '��~   M5345
1924             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1925             fnKEY_WAIT = 1
1926             MLocalLoopFlg=-1
1927             Break
1928         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1929             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1930             fnKEY_WAIT = 2
1931             MLocalLoopFlg=-1
1932             Break
1933         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1934             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1935             fnKEY_WAIT = 3
1936             MLocalLoopFlg=-1
1937             Break
1938         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1939             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1940             fnKEY_WAIT = 4
1941             MLocalLoopFlg=-1
1942             Break
1943         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1944             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1945             fnKEY_WAIT = 5
1946             MLocalLoopFlg=-1
1947             Break
1948         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1949             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1950             fnKEY_WAIT = MRobotInit1%
1951             MLocalLoopFlg=-1
1952             Break
1953         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1954             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1955             fnKEY_WAIT = MRobotInit2%
1956             MLocalLoopFlg=-1
1957             Break
1958         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1959             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1960             fnKEY_WAIT = MRobotInit3%
1961             MLocalLoopFlg=-1
1962             Break
1963         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1964             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1965             fnKEY_WAIT = MRobotInit4%
1966             MLocalLoopFlg=-1
1967             Break
1968         Else
1969         EndIf
1970     WEnd
1971     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1972     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
1973     Exit Function
1974 FEnd
1975 '
1976 '�� fnAUTO_CTL
1977 ''' <summary>
1978 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
1979 ''' </summary>
1980 ''' <remarks>
1981 ''' Date   : 2021/07/07 : M.Hayakawa
1982 ''' </remarks>
1983 Function M% fnAUTO_CTL
1984     fnAUTO_CTL = 0
1985     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1986     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
1987     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1988     '
1989     If M_Svo=0 Then             '�T�[�{ON�m�F
1990         Servo On
1991     EndIf
1992     Wait M_Svo=1
1993     Exit Function
1994 FEnd
1995 '
1996 '�� fnWindScreenOpen
1997 ''' <summary>
1998 ''' �E�B���h��ʂ̕\���A��\���ݒ�
1999 ''' </summary>
2000 '''<param name="%"></param>
2001 '''<param name="%"></param>
2002 '''<param name="%"></param>
2003 '''<param name="%"></param>
2004 ''' <remarks>
2005 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2006 ''' MWindReSet = 0     ��ʔ�\��
2007 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2008 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2009 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2010 ''' Date   : 2021/07/07 : M.Hayakawa
2011 ''' </remarks>
2012 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2013     If MCommentD1001 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2014         M_Out16(12480) = MCommentD1001      'D1001 �R�����g
2015     EndIf
2016     '
2017     If MCommentD1002 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2018         M_Out16(12496) = MCommentD1002      'D1002 �R�����g
2019     EndIf
2020     '
2021     If MCommentD1003 <> 0 Then              '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2022        M_Out16(12512) = MCommentD1003       'D1003 �R�����g
2023     EndIf
2024     '
2025     M_Out16(12448) = MScreenNo              '��ʔԍ�  M6448   10=�G���[���
2026     M_Out(12363) = 1 Dly 0.5                '�E�B���h��ʐݒ�  M6362
2027     Exit Function
2028 FEnd
2029 '
2030 '��FnCtlValue2
2031 ''' <summary>
2032 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2033 ''' </summary>
2034 ''' <param name="MCtlNo%"></param>
2035 ''' <remarks>
2036 ''' Date : 2022/04/28 �n��
2037 ''' </remarks>
2038 '''
2039 '''  1�F������       �{�P
2040 '''  2�F�g���n�j��   �{�P
2041 '''  3�F�����@�Q�z���G���[�� �{�P�@�@�g��NG����ύX 2022/05/19 �n��
2042 '''  4�F�����@�P�z���G���[�� �{�P
2043 ''' 99�F�Ǐ��J�n�M�� OFF
2044 '''
2045 Function M% FnCtlValue2(ByVal MCtlNo%)
2046     FnCtlValue2 = 1
2047     Select MCtlNo%
2048         Case 1        '�������{�P
2049             M_Out(12569) = 0             '�����݊J�n�M��OFF
2050             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2051             MInputQty = M_In16(11600)    '��������M
2052             MInputQty = MInputQty + 1    '�������{�P
2053             M_Out16(12592) = MInputQty   '���������M
2054             M_Out(12569) = 1             '�����݊J�n�M��ON
2055             Break
2056             '
2057         Case 2        '�g���n�j���{�P
2058             M_Out(12569) = 0             '�����݊J�n�M��OFF
2059             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2060             MAssyOkQty = M_In16(11616)   '�g��OK����M
2061             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2062             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2063             M_Out(12569) = 1             '�����݊J�n�M��ON
2064             Break
2065             '
2066         Case 3        '�����@�Q�z���G���[���{�P
2067             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2068             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2069             MSuctionErrQty = M_In16(11632)         '�����@�Q�z���G���[����M
2070             MSuctionErrQty = MSuctionErrQty + 1    '�����@�Q�z���G���[���{�P
2071             M_Out16(12624) = MSuctionErrQty        '�����@�Q�z���G���[�����M
2072             M_Out(12569) = 1                       '�����݊J�n�M��ON
2073             Break
2074             '
2075         Case 4        '�����@�P�z���G���[���{�P
2076             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2077             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2078             MSuctionErrQty = M_In16(11648)         '�����@�P�z���G���[����M
2079             MSuctionErrQty = MSuctionErrQty + 1    '�����@�P�z���G���[���{�P
2080             M_Out16(12640) = MSuctionErrQty        '�����@�P�z���G���[�����M
2081             M_Out(12569) = 1                       '�����݊J�n�M��ON
2082             Break
2083             '
2084         Case 99        '�Ǐ��J�n�M��OFF
2085             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2086             M_Out(12569) = 0        '�����݊J�n�M��OFF
2087             Break
2088             '
2089     End Select
2090     Exit Function
2091 FEnd
2092 '
2093 '
2094 '��FnScreEroorCord
2095 ''' �d���h���C�o�[�̃G���[�R�[�h���܂߂��R�����g���o���ׂ̃R�����g�ԍ��̍쐬
2096 ''' �V�K�쐬�F2022/05/23 : �n��
2097 '''
2098 Function M% FnScreEroorCord()
2099     MScrewErrorCord% = 0
2100     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2101     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2102     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2103     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2104     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2105     MScrewErrorCord% = MScrewErrorCord% * 10
2106     MScrewErrorCord% = MScrewErrorCord% + 500
2107     FnScreEroorCord = MScrewErrorCord%
2108     Exit Function
2109 FEnd
2110 '
2111 '
2112 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2113 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2114 '-------------------------------------------------------------------------------
2115 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2116 '   ����
2117 '       PInspPos()      �F�����ʒu
2118 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2119 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2120 '       MInspCnt%       �F�����ʒu��
2121 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2122 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2123 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2124 '   �߂�l�F����
2125 '       0=�ُ�I���A1=����I��
2126 '
2127 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2128 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2129 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2130 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2131 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2132 '-------------------------------------------------------------------------------
2133     '----- �����ݒ� -----
2134     Cnt 0                                                           '�ړ�����������(�����l=0)
2135     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2136 '    Cnt 1,0.1,0.1
2137     '�ϐ��錾�E������
2138     Def Inte MNum                                                   '�����ԍ�(������1�`)
2139     MNum% = 1                                                       '�����ԍ������l�ݒ�
2140     Def Inte MEndFlg                                                '�����I���t���O
2141     MEndFlg% = 0
2142     '
2143     '����G�ԍ��ݒ�v���E�������s�v��off
2144     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2145     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2146     '�G���[�ԍ��N���A
2147     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2148     M_Out16(MOUT_InspErrNum) = MInspErrNum
2149     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2150     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2151     '
2152     'Insight Ready check?
2153     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2154         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2155         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2156         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2157         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2158         'Exit Function
2159     EndIf
2160     If MInspErrNum = 20 Then GoTo *ISInspectionSingle_End
2161     '
2162     '�����ʒu���m�F
2163     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2164         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2165         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2166         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2167         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2168         'Exit Function
2169     EndIf
2170    If MInspErrNum = 21 Then GoTo *ISInspectionSingle_End
2171     '
2172     '
2173     '
2174     '----- ���C������ -----
2175     '�ݒ肳�ꂽ�����ʒu�����̌������s
2176     While( MEndFlg% = 0 )
2177         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2178         MSetGrNumRetryExitFlg = 0
2179         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2180         While( MSetGrNumRetryExitFlg = 0 )
2181         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2182             '
2183             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2184             '
2185             '----- �����O���[�v�ԍ��ݒ� -----
2186             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2187             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2188             '
2189             '�����ʒu�ֈړ��E�ړ������҂�
2190             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2191             Mov PInspPos( MNum% )                                       '�ړ�
2192             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2193             Dly 0.2                                                     '�ړ�������Delay 0.05>>0.2
2194             '
2195             '�����O���[�v�ԍ��ݒ�I���m�F
2196             M_Timer(1) = 0
2197             MExitFlg = 0
2198             While( MExitFlg = 0 )
2199                 '����G�ݒ萳��I��?
2200                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2201                     MExitFlg = 1
2202                 '
2203                 '����G�ݒ�ُ�I��?
2204                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2205                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2206                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2207                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2208                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2209                     EndIf
2210                     MExitFlg = 1
2211                 '
2212                 'timeout�`�F�b�N
2213                 ElseIf 1000 < M_Timer(1) Then
2214                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2215                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2216                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2217                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2218                     EndIf
2219                     MExitFlg = 1
2220                 EndIf
2221             WEnd
2222             '
2223             '����G�ԍ��ݒ�v��off
2224             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2225             '
2226             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2227             'NG�Ȃ���Δ�����
2228             If MCurrentStepErr = 0 Then
2229                 MSetGrNumRetryExitFlg = 1
2230             Else
2231                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2232                 If MSetGrNumRetryCnt = 0 Then
2233                     MSetGrNumRetryExitFlg = 1
2234                 Else
2235                     'Retry�ց@���̑O��Delay
2236                     Dly 0.5
2237                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2238                 EndIf
2239             EndIf
2240             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2241             '
2242         WEnd
2243         '
2244         '
2245         '
2246         '----- �������s -----
2247         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2248             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2249                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2250                 MInspRetryExitFlg = 0
2251                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2252                 While( MInspRetryExitFlg = 0 )
2253                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2254                     '
2255                     '���������m�F
2256                     MRetryCnt = MRetryCnt - 1
2257                     M_Timer(1) = 0
2258                     MExitFlg = 0
2259                     While( MExitFlg = 0 )
2260                     '���������҂�
2261                         '����OK�I��?
2262                         If M_In( MIN_IS_InspOK% ) = 1  Then
2263                             MJudgeOKFlg = 1                         '����OK�t���OON
2264                             MExitFlg = 1
2265                         '
2266                         '����NG�I��?
2267                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2268                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2269                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2270                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2271                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2272                                 EndIf
2273                             EndIf
2274                             MExitFlg = 1
2275                         '
2276                         '�����ُ�I��(IS timeout)?
2277                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2278                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2279                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2280                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2281                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2282                                 EndIf
2283                             EndIf
2284                             MExitFlg = 1
2285                         '
2286                         'timeout�`�F�b�N
2287                         ElseIf 3000 < M_Timer(1) Then
2288                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2289                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2290                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2291                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2292                                 EndIf
2293                             EndIf
2294                             MExitFlg = 1
2295                         EndIf
2296                     WEnd
2297                     '
2298                     '�����J�n�v��off
2299                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2300                     '
2301                     'OK�Ȃ甲����
2302                     If MJudgeOKFlg = 1 Then
2303                         MInspRetryExitFlg = 1
2304                     Else
2305                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2306                         If MRetryCnt = 0 Then
2307                             MInspRetryExitFlg = 1
2308                         Else
2309                             'Retry�ց@���̑O��Delay
2310                             Dly 0.3
2311                         EndIf
2312                     EndIf
2313                     '
2314                 WEnd
2315             EndIf
2316         EndIf
2317         '
2318         '
2319         '
2320         MNum% = MNum% + 1                                           '����Step+1
2321         '�����I���m�F�@�����I���t���O�Z�b�g
2322         If (MInspCnt% < MNum% ) Then
2323             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2324         EndIf
2325         'NG���������s������
2326         If MInspErrNum <> 0 Then                                    'NG����?
2327             If MNgContinue% <> 1 Then                               'NG���s?
2328                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2329             EndIf
2330         EndIf
2331     WEnd
2332     '
2333     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2334     If 0 < MZAxis% Then
2335         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2336         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2337         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2338         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2339     EndIf
2340     '
2341     '�߂�l�ݒ�
2342     If MInspErrNum = 0 Then
2343         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2344     Else
2345         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2346         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2347         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2348     EndIf
2349 '
2350 *ISInspectionSingle_End
2351 Fine 0 , P
2352     Exit Function
2353 FEnd
2354 '
2355 '��fnAutoScreenComment
2356 ''' <summary>
2357 ''' ���C����ʂ̓���󋵕\��
2358 ''' �R�����gD1005�̐ݒ�
2359 ''' </summary>
2360 '''<param name="McommentD1005%">�R�����gID</param>
2361 ''' <remarks>
2362 ''' Date   : 2021/07/07 : M.Hayakawa
2363 ''' </remarks>
2364 Function fnAutoScreenComment(ByVal McommentD1005%)
2365     M_Out16(12576) = McommentD1005%
2366     Exit Function
2367 FEnd
2368 '
2369 '��fnRoboPosChk
2370 ''' <summary>
2371 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2372 ''' </summary>
2373 '''<param name="MINNumber%">���͔ԍ�</param>
2374 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2375 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2376 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2377 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2378 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2379 ''' <remarks>
2380 ''' Date   : 2021/07/07 : M.Hayakawa
2381 ''' </remarks>
2382 Function M% fnRoboPosChk
2383     fnRoboPosChk = 0
2384     MRet = fnStepRead()
2385     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2386     '�E�B���h��ʐ؊���
2387     If MRBTOpeGroupNo > 5 Then
2388         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2389         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2390         Dly 0.2
2391         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2392         Dly 1.5
2393         '
2394         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2395         '
2396         MLoopFlg% = 1
2397         While MLoopFlg% = 1
2398             '
2399             '
2400             MKeyNumber% = fnKEY_WAIT()
2401             Select MKeyNumber%
2402                 Case Is = MAbout%       '��~
2403                     M_20# = MAbout%
2404                     MLoopFlg% = -1
2405                     Break
2406                 Case Is = MNext%        '����
2407                     'MLoopFlg% = -1
2408                     Break
2409                 Case Is = MContinue%    '�p��
2410                     M_20# = MContinue%
2411                     MLoopFlg% = -1
2412                     Break
2413                 Default
2414                     Break
2415             End Select
2416         WEnd
2417     EndIf
2418     '
2419     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2420         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2421         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2422         Select MRBTOpeGroupNo
2423             Case Is = 5                          '�������Ȃ�
2424                 Break
2425             Case Is = 10                         '�����ʒu�֖߂�
2426                 'Mov PTEST001
2427                 Break
2428             Case Is = 15                         '�����ʒu�֖߂�
2429                 'Mov PTEST002
2430                 Dly 0.5
2431                 'Mov PTEST001
2432                 Dly 0.5
2433                 Break
2434             Default
2435                 Break
2436         End Select
2437         '
2438         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2439         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2440         MRBTOpeGroupNo = 5
2441         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2442         Dly 1.0
2443         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2444         fnRoboPosChk = 1                        '�����ʒu������s
2445         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2446     EndIf
2447     Exit Function
2448 FEnd
2449 '
2450 '��frInCheck
2451 ''' <summary>
2452 ''' �Z���T�[IN�`�F�b�N
2453 ''' </summary>
2454 '''<param name="MINNumber%">���͔ԍ�</param>
2455 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2456 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2457 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2458 ''' <remarks>
2459 ''' Date   : 2021/07/07 : M.Hayakawa
2460 ''' </remarks>
2461 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2462     M_Timer(4) = 0
2463     MloopFlg = 0
2464     While MloopFlg = 0
2465         MCrtTime& = M_Timer(4)
2466         If M_In(MINNumber%) = MCMPFLG% Then
2467             MloopFlg = 1
2468             frInCheck = 1
2469         ElseIf MCrtTime& > MTimeCnt& Then
2470             MloopFlg = 1
2471             frInCheck = 0
2472         EndIf
2473     WEnd
2474     Exit Function
2475 FEnd
2476 '-----------------------------------------------
2477 '
2478 '�˂����ߋ@�ʐM�m�F
2479 '
2480 '-----------------------------------------------
2481 Function M% fScewTcomChk
2482     fScewTcomChk = 0
2483     '�ʐM�m�F���M
2484     M_Out(MOUT_ScwT_ComChk%) = MOn%
2485     '�ʐM�m�F��M�ҋ@
2486     Wait M_In(MIN_ScwT_comOK%) = MOn%
2487     '�ʐM�m�F���M�I��
2488     M_Out(MOUT_ScwT_ComChk%) = MOff%
2489     Exit Function
2490 FEnd
2491 '
2492 '
2493 '-----------------------------------------------
2494 '
2495 '�˂����ߊJ�n���M
2496 '
2497 '-----------------------------------------------
2498 Function M% fScewTStart
2499     fScewTStart = 0
2500     '�˂����ߊJ�n�ҋ@����M
2501     Wait M_In(MIN_ScwT_STRec%) = MOn%
2502     Dly 0.1
2503     '�˂����ߊJ�n��M�𑗐M
2504     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
2505     Exit Function
2506 FEnd
2507 '
2508 '
2509 '-----------------------------------------------
2510 '
2511 '�˂����ߊ�����M
2512 '
2513 '-----------------------------------------------
2514 Function M% fScewTFinish
2515     fScewTFinish = 0
2516     '�˂����ߊ����ҋ@����M
2517     Wait M_In(MIN_ScwT_Fin%) = MOn%
2518     Dly 0.1
2519     '�˂����ߊ�����M�𑗐M
2520     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
2521     Exit Function
2522 FEnd
2523 '
2524 '
2525 '-----------------------------------------------
2526 '
2527 '����xx��~��M
2528 '
2529 '-----------------------------------------------
2530 Function M% fScewTCaseStop(ByVal MCase%())
2531     fScewTCaseStop = 0
2532     '����xx��~����M
2533     Wait M_In(MCase%(1)) = MOn%
2534     Dly 0.1
2535     '����xx��~��M�𑗐M
2536     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
2537     Exit Function
2538 FEnd
2539 '
2540 '-----------------------------------------------
2541 '
2542 '�ĊJ�n��M
2543 '
2544 '-----------------------------------------------
2545 Function M% fScewTReStart()
2546     fScewTReStart = 0
2547     '�ĊJ�n����M
2548     Wait M_In(MIN_ScwT_ReST%) = MOn%
2549     Dly 0.1
2550     '�ĊJ�n��M�𑗐M
2551     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
2552     Exit Function
2553 FEnd
2554 '
2555 '��fErrorProcess
2556 '<summary>
2557 '�G���[����
2558 '</summary>
2559 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2560 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2561 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2562 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2563 '<make>
2564 '2021/11/5 �����V��
2565 '</make>
2566 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2567     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2568     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2569     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2570     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2571 *RETRY_ERR_PROCESS
2572      M_20# = MClear%     '������
2573 '        '�G���[�����L�q
2574         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2575 '        'GOT KEY���͑҂�
2576         MKeyNumber = fnKEY_WAIT()
2577 '        '
2578         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2579             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2580  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2581             Break
2582          '
2583         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2584             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2585  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2586         '
2587         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2588             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2589  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2590          '
2591         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2592             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2593  '           fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2594             Break
2595         '
2596         EndIf
2597         '
2598         '
2599         '
2600         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2601         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2602     Exit Function
2603 FEnd
2604 '
2605 '��InitialZone
2606 ''' <summary>
2607 ''' ���݈ʒu������ɑҔ����A�����ʒu�ɖ߂�
2608 ''' </summary>
2609 ''' <remarks>
2610 ''' Date : 2021/12/2 : M.Hayakawa
2611 ''' Update:2022/06/2 : M.Hayakawa ���H���̔���~���A�ɍ��킹�ĕύX
2612 ''' </remarks>
2613 Function fnInitialZone()
2614     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���]
2615 '
2616     Ovrd 5
2617 ' ���ޔ�
2618     PActive = P_Curr
2619     Pmove = PActive
2620 '
2621     If PActive.X > 580 Then
2622         Pmove.Z =380        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2623     Else
2624         Pmove.Z =500        '��L�ȊO��Z:500�܂Ŏ����グ
2625     EndIf
2626 '
2627     Mvs Pmove
2628     Mov PInitialPosition
2629 ' ���b�N���J��
2630     InitialState()
2631 ' ��U��~
2632     fErrorProcess(20,70,256,0)
2633     Exit Function
2634 FEnd
2635 '
2636 '��InitialState
2637 ''' <summary>
2638 ''' �n���h�A����������ʒu�ɂ���
2639 ''' </summary>
2640 ''' <returns>   0 : OK
2641 '''             1 : NG
2642 ''' </returns>
2643 ''' <remarks>
2644 ''' Date : 2021/12/2 : M.Hayakawa
2645 ''' </remarks>
2646 Function M% InitialState()
2647     InitialState = 0
2648     '���i�ʒu���߉���
2649     M_Out(12261)=1 Dly 0.3      'FAN�N�����v�o�[�p���X�o��
2650     'Wait M_In(11271)=1          'FAN�N�����v�o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2651     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'FAN�N�����v�o�[���o(8/26����)
2652     If MRtn = 0 Then
2653         fErrorProcess(11,234,284,0)
2654         Select M_20#
2655             Case MAbout%            '��~��
2656                 InitialState = 1
2657                 Break
2658             Case MNgProcess%        'NG�������ꂽ�ꍇ
2659                 InitialState = 0
2660                 Break
2661             Case MContinue%
2662                 M_20# = MClear%
2663                 InitialState = 0
2664                 Break
2665             Case MNext%
2666                 M_20# = MClear%
2667                 InitialState = 0
2668                 Break
2669         End Select
2670     EndIf
2671     '
2672     M_Out(12259)=1 Dly 0.3      '�v�b�V��CY�pSV�ߒ[�p���X�o��
2673     'Wait M_In(11269)=1          '�v�b�V���ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2674     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    '�v�b�V���ߒ[���o
2675     If MRtn = 0 Then
2676         fErrorProcess(11,234,284,0)
2677         Select M_20#
2678             Case MAbout%            '��~��
2679                 InitialState = 1
2680                 Break
2681             Case MNgProcess%        'NG�������ꂽ�ꍇ
2682                 InitialState = 1
2683                 Break
2684             Case MContinue%
2685                 M_20# = MClear%
2686                 InitialState = 0
2687                 Break
2688             Case MNext%
2689                 M_20# = MClear%
2690                 InitialState = 0
2691                 Break
2692         End Select
2693     EndIf
2694     '
2695     M_Out(12257)=1 Dly 0.3      '�ʒu����CY�pSV�ߒ[�p���X�o��
2696     'Wait M_In(11267)=1          '�ʒu���ߖߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2697     MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[���o(8/26����)
2698     If MRtn = 0 Then
2699         fErrorProcess(11,234,284,0)
2700         Select M_20#
2701             Case MAbout%            '��~��
2702                 InitialState = 1
2703                 Break
2704             Case MNgProcess%        'NG�������ꂽ�ꍇ
2705                 InitialState = 1
2706                 Break
2707             Case MContinue%
2708                 M_20# = MClear%
2709                 InitialState = 0
2710                 Break
2711             Case MNext%
2712                 M_20# = MClear%
2713                 InitialState = 0
2714                 Break
2715         End Select
2716     EndIf
2717     Exit Function
2718 FEnd
2719 '
2720 '��fnTorqueCheck
2721 ''' <summary>
2722 ''' �g���N�`�F�b�N����p�̃��C��
2723 ''' </summary>
2724 ''' <remarks>
2725 ''' Date   : 2021/12/21 : H.AJI
2726 ''' </remarks>'
2727 Function M% fnTorqueCheck
2728     '�g���N�`�F�b�N�����M  �����n��~
2729     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2730     '
2731     fnTorqueCheck = 0
2732     Ovrd 20
2733     Mov PInitialPosition              '�����ʒu�ړ�
2734     Ovrd 100
2735     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2736     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2737     Dly 0.2
2738     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2739     '
2740     'M6340  �g���N�`�F�b�N��M
2741     M_Out(12340) = 1 Dly 1.0                '�g���N�`�F�b�N��M M6340
2742     Dly 1.0
2743     M_Out(12340) = 0
2744     '
2745     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
2746     '
2747     MLoopFlg = 1
2748     While MLoopFlg = 1
2749         '
2750         Mov PInitialPosition              '�����ʒu�ړ�
2751         '
2752         MKeyNumber = fnKEY_WAIT()
2753         Select MKeyNumber
2754             Case Is = 1           '��~
2755                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
2756                 Dly 1.0
2757                 M_Out(12343) = 0
2758                 Ovrd 20
2759                 Mov PTicketRead_1
2760                 Ovrd 100
2761                 M_20# = 1
2762                 MLoopFlg = -1
2763                 Break
2764             Case Is = 2           '����
2765                 Break
2766             Case Is = 3           '�p��
2767                 Break
2768             Case Is = 4           '�g���N�`�F�b�N�J�n
2769                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
2770                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
2771                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2772                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2773                 MRet = fnMoveTorquePosi()
2774                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
2775                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2776                 Break
2777             Default
2778                 Break
2779         End Select
2780     WEnd
2781     '
2782     '�g���N�`�F�b�N����~���M
2783     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2784     '
2785     '���{�b�g�̈ʒu�����ɖ߂�
2786     '
2787     Exit Function
2788  FEnd
2789  '
2790 '
2791 '
2792 '---------------------------
2793 '
2794 '    ���C����ʂ̕\���A��\���ݒ�
2795 '         �R�����gD1001, D1002, D1003�̐ݒ�
2796 '           MWindReSet = 0     ��ʔ�\��
2797 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2798 '           MWindErrScr = 10    �G���[��� D1001, D1002
2799 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2800 '
2801 '---------------------------
2802 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2803     fnMainScreenOpen = 0
2804     '
2805    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2806         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2807     EndIf
2808     '
2809     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2810         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2811     EndIf
2812     '
2813     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2814         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2815     EndIf
2816     '
2817     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2818     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
2819     Dly 0.5
2820     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
2821     Exit Function
2822 FEnd
2823 '
2824 '��Main
2825 ''' <summary>
2826 ''' �g���N�`�F�b�N������
2827 ''' </summary>
2828 ''' <remarks>
2829 ''' Date   : 2021/12/21 : H.AJI
2830 ''' </remarks>'
2831 Function M% fnMoveTorquePosi
2832      fnMoveTorquePosi = 0
2833      Ovrd 50
2834      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
2835     '
2836     Spd M_NSpd
2837 '-------------      �h���C�o�[RST
2838     M_Out(12240)=0     '�h���C�o�[OFF CCW
2839     M_Out(12241)=0     '�h���C�o�[OFF CW
2840     M_Out(12242)=1     '�h���C�o�[���� C1
2841     M_Out(12243)=1     '�h���C�o�[���� C2
2842     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
2843 '---------------------------------------
2844 '[P-11]
2845 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
2846     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
2847     Dly 0.1
2848 '-----------------------
2849    'Cnt 0                           'Cnt����-2�@�I��
2850 '-----------------------
2851     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
2852     Dly 0.2
2853 '-----------------------
2854     ProgramBankSet(1,3)
2855     M_Out(12241)=0                   '�h���C�o�[OFF  CW
2856     'Dly 0.1
2857 '--------------------------------
2858     Ovrd 40
2859    'Dly 0.1
2860 '--------------------------------  �l�W���ߑ��x�ݒ�
2861     Spd 14                            '���C�h 100-40 100% :Spd 12
2862     Dly 0.1
2863 '--------------------------------
2864 '--------------------------------
2865 '---------------------------------�y�˂����ߓ���z
2866 '
2867     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2868    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2869     Dly 0.3                          '�������҂�
2870    M_Out(12241)=1                   '�h���C�o�[ON  CW
2871 '
2872     Wait M_In(11584)=1                '����/�G���[���o
2873     Dly 0.1
2874     Spd M_NSpd
2875    'Ovrd 20
2876     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2877     Wait M_In(11257)=1                '�l�W����SC
2878 '---------------------------------
2879     Dly 0.1
2880     M_Out(12241)=0                    '�h���C�o�[OFF CW
2881     Dly 0.1
2882     M_Out(12242)=0                    '�h���C�o�[���� C1
2883     Dly 0.1
2884     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2885     Dly 0.1
2886     M_Out(12245)=0                    '�v���O����2���� F1
2887 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2888 '
2889     Mvs PTorqueCheck,-60                       '������mov ����ύX
2890     Dly 0.1
2891 '--------------------------------------------------------------
2892    'Ovrd 80
2893 '--------------------------------------------------------------
2894 '---------------------------------------
2895 '---------------------------------------
2896 '---------------------------------------�G���[���E����
2897    *LBL1
2898    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2899    Mvs ,-100
2900    M_Out(12241)=0     '�h���C�o�[OFF CW
2901    Dly 0.1
2902    M_Out(12242)=0     '�h���C�o�[���� C1
2903    Dly 0.1
2904    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2905    Dly 0.1
2906    M_Out(12245)=0     '�v���O�������� F1
2907 '---------------------------------------
2908 '---------------------------------------
2909 '-------------
2910    'Mov PInitPos19049
2911    Dly 0.1
2912 '
2913 '
2914     Exit Function
2915 FEnd
2916 '
2917 '��Main
2918 ''' <summary>
2919 ''' �g������p�̃��C��
2920 ''' </summary>
2921 ''' <remarks>
2922 ''' Date   : 2021/07/07 : M.Hayakawa
2923 ''' </remarks>'
2924 Function Main
2925     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2926     '
2927     If M_Svo=0 Then
2928         Servo On
2929     EndIf
2930     Wait M_Svo=1
2931 '�g���X�^�[�g���t�����v���p���XON
2932     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2933 '�p�g���C�g����
2934     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2935     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2936     '
2937     M_20# = 0                                   'KEY���͏�����
2938     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
2939     MRet% = 0
2940 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2941     PActive = P_Curr                    '���݈ʒu���擾
2942     MRecoveryPass% = 0
2943     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2944         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2945             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2946             MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2947         EndIf
2948     EndIf
2949     EndIf
2950     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2951         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2952             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2953                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2954             EndIf
2955         EndIf
2956     EndIf
2957     If MRecoveryPass% = 0 Then
2958         fnInitialZone()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2959     EndIf
2960 '
2961     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2962         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2963 '�g���N�`�F�b�N
2964         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2965             MRet% = fnTorqueCheck()
2966             Break
2967         Else
2968 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
2969 '                MRtn = InspInit()               '�摜��������������
2970 '            EndIf
2971             '
2972            M_20# = MClear%                    '������
2973 '�g���J�n
2974             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2975 '                MRet% = fnAssyStart()
2976                 fnAssyStart()
2977             Else
2978                 M_20# = MPass%
2979             EndIf
2980 '�g���I�����t����
2981             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2982             Wait M_In(11572) = 1            '���t�擾����
2983             Dly 0.1
2984             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2985             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2986             fnAutoScreenComment(89)         'AUTO��� �g����������
2987 ' ��H���փt���O�o��
2988             If M_20# <> MAbout% Then
2989                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
2990             ElseIf M_20# = MPass% Then
2991                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
2992             EndIf
2993 'About(��~)�ȊO��OK���o�́i�p���b�g�~���j
2994 '            If M_20# <> MAbout% Then
2995 '                M_Out(12339) = 1 Dly 0.5    'M6339  toPLC_RBT�����p���X�o��
2996 '            EndIf
2997 '            M_Out(12346) = 0                ' M6346 toPLC_�g���J�n��M OFF
2998 'PIAS�ɑg������������
2999             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3000                 If M_20# = MPass% Then
3001                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3002                 Else
3003                     'KEY���͂�NG�̏ꍇ
3004                     If M_20# = MNgProcess% Then
3005 '                        M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3006                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3007                         MRet% = fnPiasWrite(MNG%)
3008                        nAssyNgQty = nAssyNgQty + 1
3009                     EndIf
3010                     '
3011                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3012                     If M_20# = MAssyOK% Then
3013                             '-----------------------
3014                             'D732 -> D2600 �R�s�[�v��
3015                             M_Out(12566) = 1
3016 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3017                             M_Out(12566) = 0
3018                             '
3019                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3020                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3021                             '��ԍ��ƍ�(PP�͖��g�p�j
3022 '                            MRet% = fnPCBNumberCheck()
3023                         Else
3024                             MRet% = 1
3025                         EndIf
3026                         '
3027                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3028                             If M_20# <> MAbout% Then
3029                                 '�H������OK��������
3030                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3031                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3032                                 MRet% = fnPiasWrite(MOK%)
3033                                 nAssyOkQty = 0
3034                                 nAssyOkQty = nAssyOkQty + 1
3035                             Else
3036                                 nAssyOkQty = nAssyOkQty + 1
3037                             EndIf
3038                         EndIf
3039                     EndIf
3040 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3041 '                    MRet% = fnPiasWrite(MOK%)
3042                 EndIf
3043             Else
3044                 nAssyOkQty = nAssyOkQty + 1
3045             EndIf
3046             '
3047             '�g���I�����t��������
3048             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3049             '�������A�g��OK���A�g��NG��������
3050 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3051             '
3052 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3053 '                '�摜�����I������
3054 '                MRtn = InspQuit()
3055 '            EndIf
3056         EndIf
3057         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3058     EndIf
3059 '�p�g���C�g����
3060     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3061     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3062 'GOT�\��
3063     fnAutoScreenComment(93)  'AUTO��� �H������
3064 '    M_Out(12339) = 1 Dly 0.5        ' M6339 toPLC_RBT�����p���XON
3065 '    M_Out(12346) = 0        'M6346  toPLC_AssY�J�n��M OFF
3066 '
3067 FEnd
3068 End
3069 '
3070 '
3071 '���܂��Ȃ��R�����g
3072 '��΍폜�����
3073 '
3074 '
3075 '
3076 '
3077 '
PActive=(602.000,-150.750,450.000,-180.000,-0.020,90.000)(7,0)
PCalcGetFanScrew=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PCalcGetMainScrew=(0.000,0.000,-1.200,0.000,0.000,0.000,0.000,0.000)
PGyroPcbRead=(315.060,-65.410,414.000,160.000,0.000,90.000)(7,0)
PGyroPcbRead_1=(329.620,-57.710,450.000,-180.000,0.000,90.000)(7,0)
PInitialPosition=(300.000,0.000,450.000,-180.000,0.000,-180.000)(7,0)
PMainPcbRead=(309.580,-174.170,413.000,-180.000,0.000,180.000)(7,0)
PMainPcbRead_1=(309.580,-174.170,450.000,-180.000,0.000,180.000)(7,0)
Pmove=(602.000,-150.750,380.000,180.000,-0.020,90.000)(7,0)
PParts1Check=(313.200,-30.000,435.000,180.000,0.000,-180.000)(7,0)
PParts1Check_1=(313.200,-30.000,480.000,-180.000,0.000,180.000)(7,0)
PParts2Check=(348.350,127.390,413.000,180.000,0.000,-180.000)(7,0)
PParts2Check_1=(348.350,127.390,450.000,180.000,0.000,180.000)(7,0)
PParts3Check=(538.880,54.120,420.000,180.000,0.000,-180.000)(7,0)
PParts3Check_1=(538.880,54.120,460.000,-180.000,0.000,180.000)(7,0)
PParts4Check=(520.000,-46.000,397.000,-180.000,0.000,180.000)(7,0)
PParts4Check_1=(520.000,-46.000,460.000,-180.000,0.000,-180.000)(7,0)
PScrewFan1=(316.840,123.450,303.780,-180.000,0.000,90.000)(7,0)
PScrewFan1_0=(316.840,123.450,313.780,-180.000,0.000,90.000)(7,0)
PScrewFan1_1=(316.840,123.450,370.000,-180.000,0.000,90.000)(7,0)
PScrewFan2=(348.670,91.640,303.780,-180.000,0.000,90.000)(7,0)
PScrewFan2_0=(348.670,91.640,313.780,-180.000,0.000,90.000)(7,0)
PScrewFan2_1=(348.670,91.640,370.000,-180.000,0.000,90.000)(7,0)
PScrewMain1=(304.960,-26.420,303.500,-180.000,0.000,90.000)(7,0)
PScrewMain1_0=(304.960,-26.420,309.500,-180.000,0.000,90.000)(7,0)
PScrewMain1_1=(304.960,-26.420,380.000,-180.000,0.000,90.000)(7,0)
PScrewMain2=(304.930,-175.170,304.630,-180.000,0.000,90.000)(7,0)
PScrewMain2_0=(304.930,-175.170,310.630,180.000,0.000,90.000)(7,0)
PScrewMain2_1=(304.930,-175.170,380.000,-180.000,0.000,90.000)(7,0)
PScrewMain3=(367.020,-180.760,303.900,-180.000,0.000,90.000)(7,0)
PScrewMain3_0=(367.020,-180.760,309.900,-180.000,0.000,90.000)(7,0)
PScrewMain3_1=(367.020,-180.760,380.000,-180.000,0.000,90.000)(7,0)
PScrewMain4=(379.080,23.990,303.900,-180.000,0.000,60.000)(7,0)
PScrewMain4_0=(379.080,23.990,309.900,-180.000,0.000,60.000)(7,0)
PScrewMain4_1=(379.080,23.990,380.000,-180.000,0.000,60.000)(7,0)
PScrewMain5=(384.980,-105.100,322.220,-180.000,0.000,90.000)(7,0)
PScrewMain5_0=(384.980,-105.100,333.620,-180.000,0.000,90.000)(7,0)
PScrewMain5_1=(384.980,-105.100,380.000,-180.000,0.000,90.000)(7,0)
PScrewMain6=(325.350,-79.150,323.270,180.000,0.000,90.000)(7,0)
PScrewMain6_0=(325.350,-79.150,333.670,-180.000,0.000,90.000)(7,0)
PScrewMain6_1=(325.350,-79.150,380.000,-180.000,0.000,90.000)(7,0)
PScrewSupplyFan=(233.050,389.510,338.720,-180.000,0.000,179.990)(7,0)
PScrewSupplyFan_1=(233.050,389.510,380.000,-180.000,0.000,-180.000)(7,0)
PScrewSupplyFan_2=(166.050,146.930,400.000,-180.000,0.000,128.050)(7,0)
PScrewSupplyFan_9=(127.520,411.680,432.420,-180.000,0.000,-180.000)(7,0)
PScrewSupplyMain=(102.770,194.030,338.050,180.000,0.000,-180.000)(7,0)
PScrewSupplyMain_1=(102.770,194.030,380.000,-180.000,0.000,180.000)(7,0)
PScrewSupplyMain_2=(166.050,146.930,447.340,-180.000,0.000,128.050)(7,0)
PScrewSupplyMain_9=(-3.190,216.640,432.440,180.000,0.000,-180.000)(7,0)
PTemp=(602.000,-150.750,450.000,-180.000,-0.020,90.000,0.000,0.000)(7,0)
PTicketRead=(602.000,-150.750,378.000,180.000,-0.020,90.000)(7,0)
PTicketRead_1=(602.000,-150.750,450.000,180.000,-0.020,90.000)(7,0)
PTorqueCheck=(143.660,-242.000,340.000,-180.000,-0.010,90.000)(7,0)
PTorqueCheck_1=(143.660,-242.000,360.000,-180.000,-0.010,90.000)(7,0)
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
PGetScrewPosTemp(1)=(102.770,194.030,380.000,-180.000,0.000,180.000,0.000,0.000)(7,0)
PGetScrewPosTemp(2)=(166.050,146.930,447.340,-180.000,0.000,128.050,0.000,0.000)(7,0)
PGetScrewPosTemp(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPosTemp(9)=(-3.190,216.640,432.440,180.000,0.000,-180.000,0.000,0.000)(7,0)
PGetScrewPosTemp(10)=(102.770,194.030,338.050,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInspPosition(1)=(313.200,-30.000,435.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInspPosition(2)=(348.350,127.390,413.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInspPosition(3)=(538.880,54.120,420.000,180.000,0.000,-180.000,0.000,0.000)(7,0)
PInspPosition(4)=(520.000,-46.000,397.000,-180.000,0.000,180.000,0.000,0.000)(7,0)
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
PScrewPosTemp(1)=(384.980,-105.100,380.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PScrewPosTemp(2)=(384.980,-105.100,333.620,-180.000,0.000,90.000,0.000,0.000)(7,0)
PScrewPosTemp(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPosTemp(10)=(384.980,-105.100,322.220,-180.000,0.000,90.000,0.000,0.000)(7,0)
