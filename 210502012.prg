1 ' ===================================
2 '
3 '  21050001 STEP5 Assy2�v���O����
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
43 Def Inte MOvrdA                     '�l�W����Ovrd �ϗp
44 Def Float MSpdA                     '�l�W����Spd�@�ϗp
45 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
46 '===== <Insight�ϐ��ݒ�> =====
47 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
48 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
49 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
50 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
51 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
52 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
53 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
54 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
55 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
56 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
57 'Output Signal
58 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
59 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
60 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
61 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
62 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
63 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
64 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
65 '===== <�d�h���ϐ���`> =====
66 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
67 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
68 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
69 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
70 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
71 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
72 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
73 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
74 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
75 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
76 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
77 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
78 Y60_Driver=12240 '�d�h�������v��� CCW
79 Y61_Driver=12241 '�d�h�����v��� CW
80 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
81 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
82 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
83 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
84 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
85 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
86 X34_ScrewReady1=11259 '�˂�����1�@Read
87 '===== <�d�h���萔> =====
88 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
89 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
90 Dim PEscapePosi(10)
91 MLoopCnt% = 0'
92 '===== <���{�b�g�萔> =====
93 '===== <���{�b�g�ϐ���`> =====
94 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
95 MCommentD1001 = 0
96 MCommentD1002 = 0
97 MCommentD1003 = 0
98 MScreenNo = 0
99 '
100 MCommentTSU = 0
101 MCommentTSD = 0
102 '�E�B���h��ʔԍ��ݒ�
103 MWindReSet = 0
104 MWindInfoScr = 5
105 MWindErrScr = 10
106 MWindErrScr2 = 11
107 MWindErrScr3 = 13
108 MWindErrScr17 = 17
109 MWindErrScr18 = 18
110 MWindCmmnScr = 20
111 MWindJigRelase19049 = 60
112 MWindJigRelase19050 = 61
113 MWindJigRelase19051 = 62
114 '
115 MClear% = 0        'KEY_�̃N���A
116 MAbout% = 1        'KEY_��~
117 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
118 MContinue% = 3     'KEY_�p�� �ēx����������s��
119 '
120 Def Inte MNgProcess
121 MNgProcess% = 5      'KEY_NG
122 '
123 MAssyOK% = 6       '�g������
124 MPass% = 7         '�H���p�X
125 MPiasNG% = 8       'Pias�m�F������NG
126 '
127 '�������pKEY�ԍ�   '
128 MRobotInit1% = 11  '�����ʒu�p
129 MRobotInit2% = 12  '�����ʒu�p
130 MRobotInit3% = 13  '�����ʒu�p
131 MRobotInit4% = 14  '�����ʒu�p
132 '
133 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
134 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
135 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
136 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
137 '
138 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
139 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
140 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
141 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
142 '
143 MOK% = 1               '�e����p
144 MNG% = 0               '�e����p
145 MTIMEOUT% = -1         '�e����p
146 MJudge% = 0            '������i�[�p
147 '
148 MRECIVETIME& = 0
149 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
150 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
151 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
152 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
153 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
154 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
155 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
156 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
157 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
158 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
159 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
160 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
161 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
162 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
163 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
164 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
165 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
166 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
167 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
168 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
169 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
170 MIN_PIAS_MyProcessComp% = 11573        '���H����������
171 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
172 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
173 '
174 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
175 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
176 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
177 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
178 '
179 MOUT_PiasAssyResultOK% = 12549    '�g��OK
180 MOUT_PiasAssyResultNG% = 12550    '�g��NG
181 MOUT_PiasAssyResultWr% = 12548    '�H��������������
182 '
183 MIN_PiasProcessNG% = 11559        '�H����������NG
184 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
185 MIN_PiasProcessOK% = 11558        '�H����������OK
186 '
187 MIN_Insight_Use% = 11369               '�摜�m�FON
188 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
189 '
190 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
191 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
192 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
193 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
194 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
195 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
196 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
197 '
198 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
199 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
200 '
201 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
202 '
203 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
204 '
205 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
206 MopeNo% = 0
207 MOvrdA% = 10
208 MRtn% = 0
209 MRet = 0
210 MRet3% = 0
211 '
212 Def Inte MInputQty          '������ ���Z�ϐ�
213 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
214 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
215 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
216 Def Inte nAssyOkQty         '���g�p
217 Def Inte MScrewNo
218 Def Inte MReTry
219 '===== <IO�ϐ���`> =====
220 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
221 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
222 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
223 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
224 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
225 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
226 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
227 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
228 '
229 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
230 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
231 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
232 '
233 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
234 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
235 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
236 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
237 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
238 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
239 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
240 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
241 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
242 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
243 '
244 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
245 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
246 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
247 '
248 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
249 '
250 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
251 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
252 '
253 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
254 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
255 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
256 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
257 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
258 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
259 '
260 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
261 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
262 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
263 '
264 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
265 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
266 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
267 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
268 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
269 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
270 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
271 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u '���l12250����12248�֕ύX(8/5����)
272 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
273 MOUT_VB1%   =  12250    ' �A�[����[�@�l�W�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
274 '
275 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
276 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
277 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
278 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
279 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
280 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
281 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
282 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
283 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
284 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
285 '
286 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
287 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
288 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
289 '
290 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
291 '
292 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
293 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
294 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
295 '
296 '����
297 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
298 Def Inte MOn                            '�o��=1
299 Def Inte MOff                           '�o��=0
300 '
301 '�˂����ߑ��u_�o�̓A�h���X
302 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
303 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
304 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
305 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
307 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
308 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
309 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
310 '�˂����ߑ��u_���̓A�h���X
311 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
312 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
313 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
314 Def Inte MIN_ScwT_Case1                 '����1��~����M
315 Def Inte MIN_ScwT_Case2                 '����2��~����M
316 Def Inte MIN_ScwT_Case3                 '����3��~����M
317 Def Inte MIN_ScwT_Case4                 '����4��~����M
318 Def Inte MIN_ScwT_Case5                 '����5��~����M
319 '
320 Def Inte MRetryLimit                    ' ���g���C��
321 Def Inte MRetryCount                    ' ���g���C�J�E���g
322 '
323 Dim MScwT_Case1%(2)               '����1��~�ϐ�
324 Dim MScwT_Case2%(2)               '����2��~�ϐ�
325 Dim MScwT_Case3%(2)               '����3��~�ϐ�
326 Dim MScwT_Case4%(2)               '����4��~�ϐ�
327 Dim MScwT_Case5%(2)               '����5��~�ϐ�
328 '
329 Def Pos PActive                     '�������W�n �ʒu�ϐ� ���݈ʒu
330 Def Pos Pmove                       '�������W�n �ʒu�ϐ� �ړ���
331 Def Inte MRecoveryPass              '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s'
332 '����
333 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
334 MOn% = 1                                 '�o�� = 1
335 MOff% = 0                                '�o�� = 0
336 '
337 '�˂����ߋ@_�A�h���X�ݒ�
338 MOUT_ScwT_ComChk% = 12816               '�ʐM�m�F���M
339 MOUT_ScwT_ST% = 12849                   '�˂����ߊJ�n�𑗐M
340 MOUT_ScwT_ReSTOK% = 12850               '�ĊJ�n��M�𑗐M
341 MOUT_ScwT_FinOK% = 12852                '�˂����ߊ�����M�𑗐M
342 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
343 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
344 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
345 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
346 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
347 '
348 MIN_ScwT_comOK% = 11824                 '�˂����ߑ��u����ԐM
349 MIN_ScwT_STRec% = 11857                 '�˂����ߊJ�n����M
350 MIN_ScwT_ReST% = 11858                  '�ĊJ�n����M
351 MIN_ScwT_Fin% = 11860                   '�˂����ߊ�������M
352 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
353 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
354 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
355 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
356 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
357 '
358 MScwT_Case1%(1) = MIN_ScwT_Case1%
359 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
360 MScwT_Case2%(1) = MIN_ScwT_Case2%
361 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
362 MScwT_Case3%(1) = MIN_ScwT_Case3%
363 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
364 MScwT_Case4%(1) = MIN_ScwT_Case4%
365 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
366 MScwT_Case5%(1) = MIN_ScwT_Case5%
367 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
368 '
369 MRetryLimit% = 2
370 '
371 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
372 Function M% fnAssyStart
373     M_20# = MClear%                       '������
374 '�g�ݗ��ĊJ�n
375     Ovrd 100
376 '�����ʒu��ݒ�
377     PTemp = P_Curr
378     MRtn = 0
379     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
380         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
381             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
382                 MRtn = 1
383                 Break
384             EndIf
385             Break
386         EndIf
387         Break
388     EndIf
389     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
390     If MRtn = 1 Then
391         M_Out(12262) = 1            '�ʒu���ߏoON
392         Mov PTicketRead
393         Break
394     Else
395         Mov PInitialPosition
396         M_Out(12262) = 1            '�ʒu���ߏoON
397         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
398         Mvs PTicketRead             'ID�ǂ݈ʒu
399         Break
400     EndIf
401 '
402     MRtn = 1                        'MRtn������
403     *RE_TICKET_READ
404 '    MRtn = fnPiasCheck()               'ID�ǂݎ��
405 '    PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
406 '    MInspGroup%(1) = 1              '����G�ԍ�
407 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
408     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
409         MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
410         '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
411         '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
412     EndIf
413     If MRtn = 1 Then GoTo *CompRead
414 '    fErrorProcess(11,111,254,0)
415 '    If M_20# = MNext% Then M_20# = MClear%
416 '    If M_20# = MPass% Then GoTo *AssyEnd
417 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
418 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
419     If M_20# = MContinue% Then GoTo *RE_TICKET_READ
420     If M_20# = MNext% Then M_20# = MPass%
421     GoTo *ASSY_ERROR_END
422     *CompRead
423     '
424     *INITIAL_CHECK
425     '�n���h�̏�Ԃ��C�j�V�����ɖ߂�
426     MRtn =frInCheck(11264,0,MSETTIMEOUT05&) 'PCB���o(����ƃG���[)
427     If MRtn = 1 Then GoTo *CompCheck_1
428     fErrorProcess(11,230,281,0)     '0��230�ɕύX6/8����
429     If M_20# = MNext% Then M_20# = MClear%
430     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
431     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
432     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
433     *CompCheck_1
434     '
435     If M_In(11266) = 1 Then
436         M_Out(12256) = 0        'PCB�`���b�N�JOFF
437         M_Out(12257) = 1        'PCB�`���b�N��ON
438         Break
439     EndIf
440     If M_In(11268) = 1 Then
441         M_Out(12258) = 0        'PCB�V�����_�[�oOFF
442         M_Out(12259) = 1        'PCB�V�����_�[��ON
443         Break
444     EndIf
445     If M_In(11270) = 1 Then
446         M_Out(12260) = 0        'BtoB�V�����_�[�oOFF
447         M_Out(12261) = 1        'BtoB�V�����_�[��ON
448         Break
449     EndIf
450     '
451     MRtn =frInCheck(11265,1,MSETTIMEOUT05&) 'PCB�`���b�N���o
452     If MRtn = 1 Then GoTo *CompCheck_2
453     fErrorProcess(11,240,281,0)
454     If M_20# = MNext% Then M_20# = MClear%
455     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
456     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
457     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
458     *CompCheck_2
459     '
460     MRtn =frInCheck(11267,1,MSETTIMEOUT05&) 'PCB�V�����_�[�ߌ��o
461      If MRtn = 1 Then GoTo *CompCheck_3
462     fErrorProcess(11,239,281,0)
463     If M_20# = MNext% Then M_20# = MClear%
464     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
467     *CompCheck_3
468     '
469     MRtn =frInCheck(11269,1,MSETTIMEOUT05&) 'BtoB�V�����_�[�ߌ��o
470     If MRtn = 1 Then GoTo *CompCheck_4
471     fErrorProcess(11,243,281,0)
472     If M_20# = MNext% Then M_20# = MClear%
473     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
474     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
475     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
476     *CompCheck_4
477     '
478 '---------------------------------------------------------------
479     '���͐ݒ�(�ሳ)07/30����
480     M_Out(12266) = 1
481     M_Out(12267) = 0
482 '---------------------------------------------------------------
483 '
484 '
485     '���i�ʒu����
486     *RE_POSITIONING        '�ʒu���߃��g���C�p
487     M_Out(12262)=1 Dly 0.3      '�ʒu���߃p���X�M��
488     'Wait M_In(11273)=1          '�ʒu���ߏo�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
489     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)    '�ʒu���ߏo�[���o(8/26����)
490     If MRtn = 1 Then GoTo *CompPosition_1
491     fErrorProcess(11,231,282,0)
492     If M_20# = MNext% Then M_20# = MClear%
493     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
494     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
495     If M_20# = MContinue% Then GoTo *RE_POSITIONING
496     *CompPosition_1
497     '
498     Dly 0.5
499     M_Out(12264)=1 Dly 0.3      '�v�b�V���p���X�M��
500     'Wait M_In(11275)=1          '�v�b�V���o�[���o(�C���ɂ��R�����g�A�E�g(8/26����))
501     MRtn = frInCheck(11275,1,MSETTIMEOUT05&)    '�v�b�V���ʒu�o�[���o(8/26����)
502     If MRtn = 1 Then GoTo *CompPosition_2
503     fErrorProcess(11,232,282,0)
504     If M_20# = MNext% Then M_20# = MClear%
505     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
506     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
507     If M_20# = MContinue% Then GoTo *RE_POSITIONING
508     *CompPosition_2
509 '
510     'SOC������
511     *RE_GET_SOC
512     '
513     Mov PSocGet_2               '��s�b�N�A�b�v���_  Y:�ύX 107.160��106.160
514     M_Out(12256)=0              '��`���b�N�JOFF
515     M_Out(12257)=1              '��`���b�N��ON
516     '
517     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    '�`���b�N�Z���T�[ON
518     If MRtn = 1 Then GoTo *CompGetSOC_1
519     fErrorProcess(11,240,284,0)
520     If M_20# = MNext% Then M_20# = MClear%
521     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
522     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
523     If M_20# = MContinue% Then GoTo *RE_GET_SOC
524     *CompGetSOC_1
525     '
526     M_Out(12259)=0              'PCB�V�����_�[��OFF
527     M_Out(12258)=1              'PCB�V�����_�[�oON
528     Dly 0.2
529     '
530 '    Wait M_In(11268)=1          'PCB�V�����_�[�o�[�Z���T�[ON
531     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)
532     If MRtn = 1 Then GoTo *CompGetSOC_2
533     fErrorProcess(11,238,284,0)
534     If M_20# = MNext% Then M_20# = MClear%
535     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
536     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
537     If M_20# = MContinue% Then GoTo *RE_GET_SOC
538     *CompGetSOC_2
539     '
540     Mov PSocGet_1               '���� Y:�ύX 107.160��106.160
541     Ovrd 40
542     Mvs PSocGet                 '��s�b�N�A�b�v�ʒu  Y:�ύX 107.170��106.170
543     Dly 0.3
544     Ovrd 5                      '2021-12-19�ǉ� AJ
545     '
546     '
547 '    Wait M_In(11264)=1          'PCB���o�Z���T�[ON
548     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
549     If MRtn = 1 Then GoTo *CompGetSOC_3
550     fErrorProcess(11,299,291,0)     '0,284��299.291�ɕύX6/8����
551     If M_20# = MNext% Then M_20# = MClear%
552     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
553     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
554     If M_20# = MContinue% Then GoTo *RE_GET_SOC
555     *CompGetSOC_3
556     '
557     M_Out(12257)=0              '��`���b�N��OFF
558     M_Out(12256)=1              '��`���b�N�JON
559     Dly 0.2
560     '
561 '    Wait M_In(11266)=1          '�`���b�N�J�Z���T�[ON
562     MRtn = frInCheck(11266 , 1 , MSETTIMEOUT05&)
563     Mvs PSocGet_1               '����  Y:�ύX 107.160��106.160
564     If MRtn = 1 Then GoTo *CompGetSOC_4
565     Mov PSocGet_2               ' Y:�ύX 107.160��106.160
566     fErrorProcess(11,241,284,0)
567     If M_20# = MNext% Then M_20# = MClear%
568     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
569     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
570     If M_20# = MContinue% Then GoTo *RE_GET_SOC
571     *CompGetSOC_4
572     '
573     'Wait M_In(11264)=1          'PCB���o�Z���T�[ON
574     '
575     '���L�APCB����ƃG���[�����ǉ� 2021-12-19 AJ
576     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
577     If MRtn = 1 Then GoTo *CompGetSOC_41
578     fErrorProcess(11,299,291,0)     '0,284��299.291�ɕύX6/8����
579     If M_20# = MNext% Then M_20# = MClear%
580     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
581     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
582     If M_20# = MContinue% Then GoTo *RE_GET_SOC
583     *CompGetSOC_41
584     '
585     '
586     'Ovrd 100                   '�ʒu�ύX2021-12-19�ǉ�AJ
587     Ovrd 15                     '�ǉ�1/17����
588     Mov PSocGet_2               '��s�b�N�A�b�v���_
589     Ovrd 100                    '2021-12-19�ǉ�AJ
590     '
591     'SOC��𐻕i��ɒu��
592     Mov PSocSet_2               '��u�����_
593     Mov PSocSet_1               '���i���
594     Dly 0.1
595     Ovrd 40
596     Mvs PSocSet                 '��u���ʒu�i�󒆂ŗ����j
597     M_Out(12256)=0              '��`���b�N�JOFF
598     M_Out(12257)=1              '��`���b�N��ON
599 '
600     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
601     Mvs PSocSet_1               '���i���
602     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
603 '
604     'Wait M_In(11265)=1          '�`���b�N�Z���T�[ON
605     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)
606     If MRtn = 1 Then GoTo *CompGetSOC_5
607     fErrorProcess(11,240,284,0)
608     If M_20# = MNext% Then M_20# = MClear%
609     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
610     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
611     If M_20# = MContinue% Then GoTo *RE_GET_SOC
612     *CompGetSOC_5
613     '
614 '    Wait M_In(11268)=1          'PCB�V�����_�[�o�[�Z���T�[ON�E�E�E����OFF���������}�������s���Ă���
615     MRtn = frInCheck(11268 , 1 , MSETTIMEOUT05&)
616     If MRtn = 1 Then GoTo *CompGetSOC_6
617     fErrorProcess(11,238,284,0)
618     If M_20# = MNext% Then M_20# = MClear%
619     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
620     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
621     If M_20# = MContinue% Then GoTo *RE_GET_SOC
622     *CompGetSOC_6
623     '
624     'Wait M_In(11264)=0          'PCB���o�Z���T�[OFF
625     M_Out(12258)=0              'PCB�V�����_�[�oOFF
626     M_Out(12259)=1              'PCB�V�����_�[��ON
627     '
628 '    Wait M_In(11267)=1          'PCB�V�����_�[�ߒ[�Z���T�[ON
629     MRtn = frInCheck(11267 , 1 , MSETTIMEOUT05&)
630     If MRtn = 1 Then GoTo *CompGetSOC_7
631     fErrorProcess(11,239,284,0)
632     If M_20# = MNext% Then M_20# = MClear%
633     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
634     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
635     If M_20# = MContinue% Then GoTo *RE_GET_SOC
636     *CompGetSOC_7
637     Ovrd 100
638     Mov PSocSet_2               '��u�����_
639 '�ySOC���ID�ǂݍ��݁z
640     *RE_SOC_CHECK1
641     PInspPosition(1) = PSocPcbRead  'SOC���ID�ǎ�ʒu
642     MInspGroup%(1) = 2              '����G�ԍ�
643     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
644 '
645     If MRtn = 1 Then GoTo *CompSocCheck1
646     fErrorProcess(11,97,25,0)
647     If M_20# = MNext% Then M_20# = MClear%
648     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
649     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
650     If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
651     *CompSocCheck1
652 '�y���ID�R�s�[�z
653     *RE_PCB_RECORD
654     M_Out(12571) = 1    ' �̈�1 ��ԍ��R�s�[ (D2600-) On
655     Dly 0.1
656     M_Out(12566) = 1    ' toPLC_��ԍ��R�s�[�v�� On
657 '
658     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��R�s�[���� On
659     If MRtn = 1 Then
660         M_Out(12571) = 0  ' �̈�1 ��ԍ��R�s�[ (D2600-) Off
661         Dly 0.1
662         M_Out(12566) = 0  ' toPLC_��ԍ��R�s�[�v�� Off
663 '        GoTo *RE_PCB_COMPAIRE   ' ��ԍ��ƍ��ɃX�L�b�v
664     Else
665         fErrorProcess(11,39,25,0)
666         If M_20# = MNext% Then M_20# = MClear%
667         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
668         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
669         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
670     EndIf
671 '�y���ID�ƍ��i�R�t���j�z
672     MRetryCount% = 0
673     While (MRetryCount% <= MRetryLimit%)
674         *RE_PCB_COMPAIRE
675         M_Out(12557)= 1 ' ��ԍ��ƍ��r�b�gON
676         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_��ԍ��ƍ�OK(M420) On
677         If MRtn = 1 Then
678             M_Out(12557)= 0     ' ��ԍ��ƍ��r�b�gOff
679             ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
680             MRetryCount% = 99
681         Else
682             If MRetryCount% = MRetryLimit% Then
683                 If M_In(11565) = 1 Then
684                     fErrorProcess(11,37,25,0)
685                 Else
686                     fErrorProcess(11,38,25,0)
687                 EndIf
688                 If M_20# = MNext% Then
689                     M_20# = MClear%
690                     ' ���g���C�񐔐ݒ�Ń��[�v�𔲂���
691                     MRetryCount% = 99
692                 EndIf
693                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
694                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
695                 If M_20# = MContinue% Then
696                     MRetryCount% = 0
697                 EndIf
698             Else
699                 ' ���g���C�񐔃C���N�������g
700                 MRetryCount% = MRetryCount% + 1
701                 Dly 0.5 ' ���̍H���ƃ^�C�~���O�����炷�ׂ̃f�B���C
702             EndIf
703         EndIf
704     WEnd
705 '
706 '�ySoc��摜�`�F�b�N�z
707 '    *RE_SOC_CHECK2
708 '    PInspPosition(1) = PSocCheck    'Soc��摜�`�F�b�N�ʒu
709 '    MInspGroup%(1) = 3              '����G�ԍ�
710 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
711 '    If MRtn = 1 Then GoTo *CompSocCheck2
712 '    fErrorProcess(11,43,46,0)
713 '    If M_20# = MNext% Then M_20# = MClear%
714 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
715 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
716 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK2
717 '    *CompSocCheck2
718     '��u���ʒu�摜�����i�s�v�H�j
719     '
720     'SOC���BtoB�v���X
721     'Mov PSocPress_2             'BtoB�v���X���_
722     *RE_BtoBPRESS   'BtoB�V�����_�[���g���C
723     Mov PSocPress_1             '�v���X���
724     M_Out(12261)=0              '�v���X�V�����_�[��OFF
725     M_Out(12260)=1              '�v���X�V�����_�[�oON
726     Dly 0.2
727     '
728     'Wait M_In(11270)=1          '�v���X�V�����_�[�o�[�Z���T�[ON(�C���ɂ��R�����g�A�E�g(8/27����))
729     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    '�v���X�V�����_�[�o�[�Z���T�[ON(8/27����)
730     If MRtn = 1 Then GoTo *CompPress_1
731     fErrorProcess(11,242,284,0)
732     If M_20# = MNext% Then M_20# = MClear%
733     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
734     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
735     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
736     *CompPress_1
737 '
738 '--------------------------------------------
739     '���͌��o(�ሳ��)
740     '22/07/29�ǉ� ����
741 '--------------------------------------------
742 *RE_Pa_OUT
743     If M_20# = MContinue% Then
744     M_Out(12266) = 1
745     M_Out(12267) = 0
746     Dly 0.5
747     M_20# = MClear%
748     EndIf
749     MRtn = frInCheck(11277,1,MSETTIMEOUT05&)     'MDV�p���͌��o(22/07/29����)
750     MRtn2 = frInCheck(11278,0,MSETTIMEOUT05&)    'KA�p���͌��o(ON�ŏオ�肷��)(22/07/29����)
751     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompPaOut
752     If MRtn = 0 Then
753         fErrorProcess(11,200,201,0)
754     ElseIf MRtn2 = 0 Then
755         fErrorProcess(11,200,201,0)
756     EndIf
757     If M_20# = MNext% Then M_20# = MClear%
758     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
759     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
760     If M_20# = MContinue% Then GoTo *RE_Pa_OUT
761 *CompPaOut
762     '
763     Ovrd 40
764     Mvs PSocPress               '�v���X�G���h�[�܂ňړ�
765     Dly 0.5
766     'Wait M_In(11270)=1          '�v���X�V�����_�[�o�[�Z���T�[ON�c����OFF��������R�l�N�^�J�o�[�L
767     MRtn = frInCheck(11270,0,MSETTIMEOUT05&)    '�v���X�V�����_�[�o�[�Z���T�[ON(8/27����)
768     If MRtn = 1 Then GoTo *CompPress_2
769     Mvs PSocPress_1              '�v���X���
770     fErrorProcess(11,70,71,0)
771     If M_20# = MNext% Then M_20# = MClear%
772     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
773     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
774     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
775     *CompPress_2
776     '
777     'Dly 0.2
778     '
779     *RE_BtoB_REST
780     M_Out(12260)=0              '�v���X�V�����_�[�oOFF
781     M_Out(12261)=1              '�v���X�V�����_�[��ON
782 '    Wait M_In(11269)=1          '�v���X�V�����_�[�ߒ[�Z���T�[ON
783     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    '�v���X�V�����_�[�ߒ[�Z���T�[ON(8/27����)
784     If MRtn = 1 Then GoTo *CompBtoBRest
785     fErrorProcess(11,243,284,0)
786     If M_20# = MNext% Then M_20# = MClear%
787     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
788     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
789     If M_20# = MContinue% Then GoTo *RE_BtoB_REST
790     *CompBtoBRest
791     '
792     Ovrd 100
793     Mov PSocPress_1             '�v���X���
794     M_Out(12266) = 0
795     M_Out(12267) = 0
796     'Mov PSocPress_2            'BtoB�v���X���_
797     '
798 'Soc��l�W����
799     PGetScrewPos(1) = PScrewSupply_1        ' �˂��s�b�N�A�b�v������
800     PGetScrewPos(2) = PScrewSupply_2        ' �˂������@���_����
801     PGetScrewPos(9) = PScrewSupply_9        ' �l�W�����@���l�W�̂Ĉʒu(10/6 M.H�ǉ�)
802     PGetScrewPos(10) = PScrewSupply         ' �˂��s�b�N�A�b�v������
803     '
804     'Soc��p�l�W�����@�փl�W�����ɍs��
805     *RE_SCREW_GET_1
806     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
807     If MRtn = 1 Then GoTo *CompScrewGet_1
808     If M_20# = MNext% Then M_20# = MClear%
809     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
810     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
811     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
812     *CompScrewGet_1
813     '
814     PScrewPos(1) = PScrewSoc1_1             ' �˂��s�b�N�A�b�v������
815     PScrewPos(2) = PScrewSoc1_0             ' �l�W1���ߊJ�n�ʒu����(10/8 M.H)
816     PScrewPos(10) = PScrewSoc1              ' �˂������@���_����
817     '�@�ԃl�W����
818     M_Out16(12672) = 1                      '�l�W���߈ʒu�ԍ����M
819     MRtn = ScrewTight(PScrewPos,1,10.0)
820     M_Out16(12672) = 0                      '�l�W���߈ʒu�ԍ��N���A
821     If MRtn = 1 Then GoTo *CompScrew1
822     Mov PInitialPosition
823     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
824     MScrewErrorCord% = MScrewErrorCord% + 1
825     fErrorProcess(11,MScrewErrorCord%,52,0)
826 '    fErrorProcess(11,53,52,0)
827     If M_20# = MNext% Then M_20# = MClear%
828     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
829     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
830     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
831     *CompScrew1
832     '
833     'Soc��p�l�W�����@�փl�W�����ɍs��
834     *RE_SCREW_GET_2
835     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
836     If MRtn = 1 Then GoTo *CompScrewGet_2
837     If M_20# = MNext% Then M_20# = MClear%
838     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
839     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
840     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
841     *CompScrewGet_2
842     '
843     PScrewPos(1) = PScrewSoc2_1             ' �˂��s�b�N�A�b�v������
844     PScrewPos(2) = PScrewSoc2_0             ' �l�W2���ߊJ�n�ʒu����(10/8 M.H)
845     PScrewPos(10) = PScrewSoc2              ' �˂������@���_����
846     '�A�ԃl�W����
847     M_Out16(12672) = 2                      '�l�W���߈ʒu�ԍ����M
848     MRtn = ScrewTight(PScrewPos,1,10.0)
849     M_Out16(12672) = 0                      '�l�W���߈ʒu�ԍ��N���A
850     If MRtn = 1 Then GoTo *CompScrew2
851     Mov PInitialPosition
852     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
853     MScrewErrorCord% = MScrewErrorCord% + 2
854     fErrorProcess(11,MScrewErrorCord%,52,0)
855 '    fErrorProcess(11,54,52,0)
856     If M_20# = MNext% Then M_20# = MClear%
857     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
858     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
859     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
860     *CompScrew2
861     '
862     'Soc��p�l�W�����@�փl�W�����ɍs��
863     *RE_SCREW_GET_3
864     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
865     If MRtn = 1 Then GoTo *CompScrewGet_3
866     If M_20# = MNext% Then M_20# = MClear%
867     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
868     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
869     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
870     *CompScrewGet_3
871     '
872     PScrewPos(1) = PScrewSoc3_1             ' �˂��s�b�N�A�b�v������
873     PScrewPos(2) = PScrewSoc3_0             ' �l�W3���ߊJ�n�ʒu����(10/8 M.H)
874     PScrewPos(10) = PScrewSoc3              ' �˂������@���_����
875     '�B�ԃl�W����
876     M_Out16(12672) = 3                      '�l�W���߈ʒu�ԍ����M
877     MRtn = ScrewTight(PScrewPos,1,10.0)
878     M_Out16(12672) = 0                      '�l�W���߈ʒu�ԍ��N���A
879     If MRtn = 1 Then GoTo *CompScrew3
880     Mov PInitialPosition
881     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
882     MScrewErrorCord% = MScrewErrorCord% + 3
883     fErrorProcess(11,MScrewErrorCord%,52,0)
884 '    fErrorProcess(11,55,52,0)
885     If M_20# = MNext% Then M_20# = MClear%
886     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
887     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
888     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
889     *CompScrew3
890     '
891     'Soc��p�l�W�����@�փl�W�����ɍs��
892     *RE_SCREW_GET_4
893     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
894     If MRtn = 1 Then GoTo *CompScrewGet_4
895     If M_20# = MNext% Then M_20# = MClear%
896     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
897     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
898     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
899     *CompScrewGet_4
900     '
901     PScrewPos(1) = PScrewSoc4_1             ' �˂��s�b�N�A�b�v������
902     PScrewPos(2) = PScrewSoc4_0             ' �l�W4���ߊJ�n�ʒu����(10/8 M.H)
903     PScrewPos(10) = PScrewSoc4              ' �˂������@���_����
904     '�C�ԃl�W����
905     M_Out16(12672) = 4                      '�l�W���߈ʒu�ԍ����M
906     MRtn = ScrewTight(PScrewPos,1,10.0)
907     M_Out16(12672) = 0                      '�l�W���߈ʒu�ԍ��N���A
908     If MRtn = 1 Then GoTo *CompScrew4
909     Mov PInitialPosition
910     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
911     MScrewErrorCord% = MScrewErrorCord% + 4
912     fErrorProcess(11,MScrewErrorCord%,52,0)
913 '    fErrorProcess(11,56,52,0)
914     If M_20# = MNext% Then M_20# = MClear%
915     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
916     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
917     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
918     *CompScrew4
919     '
920     'Soc��p�l�W�����@�փl�W�����ɍs��
921     *RE_SCREW_GET_5
922     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
923     If MRtn = 1 Then GoTo *CompScrewGet_5
924     If M_20# = MNext% Then M_20# = MClear%
925     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
926     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
927     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
928     *CompScrewGet_5
929     '
930     PScrewPos(1) = PScrewSoc5_1             ' �˂��s�b�N�A�b�v������
931     PScrewPos(2) = PScrewSoc5_0             ' �l�W5���ߊJ�n�ʒu����(10/8 M.H)
932     PScrewPos(10) = PScrewSoc5              ' �˂������@���_����
933     '�D�ԃl�W����
934     M_Out16(12672) = 5                      '�l�W���߈ʒu�ԍ����M
935     MRtn = ScrewTight(PScrewPos,1,10.0)
936     M_Out16(12672) = 0                      '�l�W���߈ʒu�ԍ��N���A
937     If MRtn = 1 Then GoTo *CompScrew5
938     Mov PInitialPosition
939     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
940     MScrewErrorCord% = MScrewErrorCord% + 5
941     fErrorProcess(11,MScrewErrorCord%,52,0)
942 '    fErrorProcess(11,57,52,0)
943     If M_20# = MNext% Then M_20# = MClear%
944     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
945     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
946     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
947     *CompScrew5
948     '
949     'Soc��p�l�W�����@�փl�W�����ɍs��
950     *RE_SCREW_GET_6
951     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
952     If MRtn = 1 Then GoTo *CompScrewGet_6
953     If M_20# = MNext% Then M_20# = MClear%
954     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
955     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
956     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
957     *CompScrewGet_6
958     '
959     PScrewPos(1) = PScrewSoc6_1             ' �˂��s�b�N�A�b�v������
960     PScrewPos(2) = PScrewSoc6_0             ' �l�W6���ߊJ�n�ʒu����(10/8 M.H)
961     PScrewPos(10) = PScrewSoc6              ' �˂������@���_����
962     '�E�ԃl�W����
963     M_Out16(12672) = 6                      '�l�W���߈ʒu�ԍ����M
964     MRtn = ScrewTight(PScrewPos,1,10.0)
965     M_Out16(12672) = 0                      '�l�W���߈ʒu�ԍ��N���A
966     If MRtn = 1 Then GoTo *CompScrew6
967     Mov PInitialPosition
968     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
969     MScrewErrorCord% = MScrewErrorCord% + 6
970     fErrorProcess(11,MScrewErrorCord%,52,0)
971 '    fErrorProcess(11,58,52,0)
972     If M_20# = MNext% Then M_20# = MClear%
973     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
974     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
975     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
976     *CompScrew6
977 '
978     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
979     Mov PTicketRead_1   '�`�P�b�g�ǂݎ��ʒu���
980     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
981     InitialState()  ' ������Ԃɂ���*AssyEnd
982     M_20# = MAssyOK%              ' ����I������
983     GoTo *fnAssyStart_FEndPosi
984 '
985 *ASSY_ERROR_END
986     fnInitialZone()   ' �����ʒu�Ɉړ�
987     InitialState()  ' ������Ԃɂ���*AssyEnd
988 *AssyEnd
989 *fnAssyStart_FEndPosi
990     Exit Function
991 FEnd
992 '
993 '��fnPiasCheck
994 ''' <summary>
995 ''' PIAS�`�P�b�g�Ǎ���
996 ''' </summary>
997 ''' <returns>   0 : NG
998 '''             1 : OK(�Ǎ��݊���)
999 ''' </returns>
1000 ''' <remarks>
1001 ''' Date   : 2021/07/07 : M.Hayakawa
1002 ''' </remarks>'
1003 Function M% fnPiasCheck
1004     fnPiasCheck = 0
1005     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1006     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1007 '
1008 *RETRY_PIAS
1009     M_20# = MClear%
1010     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1011     '
1012     '�yID�`�P�b�g�ǂݍ��݁z
1013     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1014     MInspGroup%(1) = 1              '����G�ԍ�
1015     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1016 '
1017     '�G���[�̏ꍇ
1018     If MRtn <> 1 Then
1019         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1020         If MRtn <> 1 Then
1021             'D720 -> D1300 �R�s�[�v��
1022             M_Out(12565) = 1
1023             Dly 0.5
1024             M_Out(12565) = 0
1025             '�G���[�����L�q
1026             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1027             'GOT KEY���͑҂�
1028             MKeyNumber = fnKEY_WAIT()
1029             '
1030             Select MKeyNumber
1031                 Case MNext%         '���ւ�I�������ꍇ
1032                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1033                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1034                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1035                     Break
1036                 Case MAbout%        '��~��I�������ꍇ
1037                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1038                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1039                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1040                     Break
1041                 Case MNgProcess%    'NG��I�������ꍇ
1042                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1043                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1044                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1045                     Break
1046                 Case MContinue%     '�p����I�������ꍇ
1047                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1048                     M_20# = MContinue%
1049                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1050                     Break
1051             End Select
1052         EndIf
1053     EndIf
1054 '----------D720 -> D1300 �R�s�[�v��----------
1055     M_Out(12565) = 1
1056     Dly 0.5
1057     M_Out(12565) = 0
1058 '----------�ʐM�m�F������----------
1059     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1060     MRtn = 0                ' ������
1061     M_20# = MClear%         ' ������
1062     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1063     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1064     If MRtn <> 1 Then
1065         If M_20# = MContinue% Then
1066             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1067         Else
1068             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1069         EndIf
1070     EndIf
1071 '----------�H�������m�F----------
1072     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1073     MRtn = 0                ' ������
1074     M_20# = MClear%         ' ������
1075     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1076     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1077     If MRtn <> 1 Then
1078         If M_20# = MContinue% Then
1079             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1080         Else
1081             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1082         EndIf
1083     EndIf
1084     '
1085     fnPiasCheck = 1
1086     *fnPiasCheck_End
1087     Exit Function
1088 FEnd
1089 '
1090 '��fnPCComuCheck
1091 ''' <summary>
1092 ''' PC-PLC�ʐM�`�F�b�N
1093 ''' </summary>
1094 ''' <returns>   0 : NG
1095 '''             1 : OK(�Ǎ��݊���)
1096 ''' </returns>
1097 ''' <remarks>
1098 ''' Date   : 2021/07/07 : M.Hayakawa
1099 ''' </remarks>'
1100 Function M% fnPCComuCheck
1101     fnPCComuCheck = 0
1102     MJudge% = 0                                  '������
1103     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1104     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1105     '
1106     For MStaNo = 0 To 5
1107         '
1108         If M_In(MIN_PIAS_ComOK%) = 1 Then
1109             'PC�ʐMOK(M400)
1110             MJudge% = MOK%
1111             MStaNo = 5
1112             Break
1113         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1114             'toRBT_�ʐM�m�Ftime out
1115             MJudge% = MNG%
1116             MCommentD1001 = 15
1117             MCommentD1002 = 21
1118             MStaNo = 5
1119             Break
1120         Else
1121             'toRBT_�ʐM�m�Ftime out
1122             MJudge% = MNG%
1123             MCommentD1001 = 14
1124             MCommentD1002 = 21
1125             Break
1126         EndIf
1127     Next MStaNo
1128     '
1129     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1130     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1131     '
1132     '�G���[���
1133     If MJudge% <> MOK% Then
1134         M_20# = MClear%     '������
1135         '�G���[�����L�q
1136         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1137         'GOT KEY���͑҂�
1138         MKeyNumber = fnKEY_WAIT()
1139         '
1140         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1141             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1142             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1143             Break
1144         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1145             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1146             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1147             Break
1148         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1149             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1150             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1151             Break
1152         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1153             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1154             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1155             Break
1156         EndIf
1157     Else
1158         'OK�̏ꍇ
1159         fnPCComuCheck = 1
1160     EndIf
1161     Exit Function
1162 FEnd
1163 '
1164 '��fnProcessCheck
1165 ''' <summary>
1166 ''' �H�������m�F
1167 ''' </summary>
1168 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1169 '''             -1�F�O�H������NG  -2�F���H����������
1170 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1171 '''             -5�F���������G���[
1172 ''' </returns>
1173 ''' <remarks>
1174 ''' Date   : 2021/07/07 : M.Hayakawa
1175 ''' </remarks>'
1176 Function M% fnProcessCheck
1177     fnProcessCheck = 0
1178     MJudge% = MNG%      '��UNG���������Ƃ���
1179 '----------�H�������m�F----------
1180     MCommentD1001 = 0   '�R�����g������
1181     For MStaNo = 0 To 5
1182         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1183         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1184         '
1185         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1186             MJudge% = MOK%
1187             fnAutoScreenComment(85)     ' AUTO���
1188             MStaNo = 5
1189             Break
1190         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1191             MFlgLoop% = 0
1192             MJudge% = MNG%
1193             MCommentD1001 = 27
1194             MCommentD1002 = 22
1195             fnAutoScreenComment(94)     ' AUTO���
1196             fnProcessCheck = -2         ' NG��-2��Ԃ�
1197             MStaNo = 5
1198             Break
1199         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1200            MJudge% = MNG%
1201             MCommentD1001 = 31
1202             MCommentD1002 = 22
1203             fnAutoScreenComment(83)     ' AUTO���
1204             fnProcessCheck = -3         ' NG��-3��Ԃ�
1205             MStaNo = 5
1206             Break
1207         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1208             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1209             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1210             MJudge% = MNG%
1211             MCommentD1001 = 32
1212             MCommentD1002 = 22
1213             fnAutoScreenComment(84)     ' AUTO���
1214             fnProcessCheck = -1         ' NG��-1��Ԃ�
1215             Dly 1.0
1216             '�H�������m�FOFF
1217             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1218             Dly 1.0
1219            'MStaNo = 5
1220             Break
1221         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1222             MFlgLoop% = 0
1223             MJudge% = MNG%
1224             MCommentD1001 = 29
1225             MCommentD1002 = 22
1226             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1227             fnProcessCheck = -5         ' NG��-5��Ԃ�
1228             MStaNo = 5
1229             Break
1230         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1231             MJudge% = MNG%
1232             If MCommentD1001 = 32 Then
1233                 '�������Ȃ�
1234             Else
1235                 MCommentD1001 = 26
1236             EndIf
1237             MCommentD1002 = 22
1238             fnProcessCheck = -4         ' NG��-4��Ԃ�
1239             MStaNo = 5
1240             Break
1241         Else
1242             MJudge% = MNG%
1243             MCommentD1001 = 28
1244             MCommentD1002 = 22
1245         EndIf
1246     Next MStaNo
1247     '�H�������m�FOFF
1248     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1249     '�ʉߗ���NG �H�������̏ꍇ
1250     If MJudge% = MPass% Then
1251         M_20# = MPass%
1252     EndIf
1253     '
1254     '�G���[���
1255     If MJudge% <> MOK% Then
1256         M_20# = MClear%     '������
1257         '�G���[�����L�q
1258         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1259         'GOT KEY���͑҂�
1260         MKeyNumber = fnKEY_WAIT()
1261         '
1262         Select MKeyNumber
1263             Case MAbout%        '��~��I�������ꍇ
1264                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1265                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1266                 Break
1267             Case MNext%         '���ւ�I�������ꍇ
1268                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1269                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1270                 Break
1271             Case MContinue%     '�p����I�������ꍇ
1272                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1273                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1274                 Break
1275             Case MNgProcess%    'NG��I�������ꍇ
1276                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1277                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1278                 Break
1279         End Select
1280     Else
1281         fnProcessCheck = 1  ' OK��1��Ԃ�
1282     EndIf
1283     Exit Function
1284 FEnd
1285 '
1286 '��fnPiasWrite
1287 ''' <summary>
1288 ''' Pias �g�����ʏ����ݗv��
1289 ''' </summary>
1290 '''<param name="MFlg%">
1291 '''                 MOK%(1) = �H��������OK��������
1292 '''                 MNG%(0) = �H��������NG��������
1293 '''</param>
1294 '''<returns></returns>
1295 ''' <remarks>
1296 ''' Date   : 2021/07/07 : M.Hayakawa
1297 ''' </remarks>'
1298 Function M% fnPiasWrite(ByVal MFlg%)
1299       fnPiasWrite = 0
1300 *RETRY_PIASWRITE
1301     '
1302     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1303    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1304     If MFlg% = MOK% Then
1305         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1306     Else
1307         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1308     EndIf
1309     Dly 0.1                  '�O�̂���
1310     '
1311     'Pias�֏����݊J�n M305 -> ON
1312     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1313     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1314     '
1315     MJudge% = MNG%
1316     '
1317     For MStaNo = 0 To 5
1318         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1319             MJudge% = MOK%
1320             'MRet = fnAutoScreenComment(85)  'AUTO���
1321             MStaNo = 5
1322             Break
1323         '
1324         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1325             MJudge% = MNG%
1326             'MRet = fnAutoScreenComment(85)  'AUTO���
1327            MCommentD1001 = 34
1328            MCommentD1002 = 25
1329             MStaNo = 5
1330             Break
1331         '
1332         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1333             MJudge% = MNG%
1334             'MRet = fnAutoScreenComment(85)  'AUTO���
1335            MCommentD1001 = 35
1336            MCommentD1002 = 25
1337             MStaNo = 5
1338             Break
1339         '
1340         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1341             MJudge% = MNG%
1342             'MRet = fnAutoScreenComment(85)  'AUTO���
1343            MCommentD1001 = 36
1344            MCommentD1002 = 25
1345             MStaNo = 5
1346             Break
1347         '
1348         Else
1349             MJudge% = MNG%
1350            MCommentD1001 = 42
1351            MCommentD1002 = 25
1352         '
1353         EndIf
1354         '
1355     Next MStaNo
1356     '
1357     'Pias�֏����݊J�n M305 -> OfF
1358     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1359     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1360     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1361     '
1362     '
1363     '�ʉߗ���NG �H�������̏ꍇ
1364     If MJudge% = MPass% Then
1365         M_20# = MPass%
1366     EndIf
1367     '
1368    M_20# = MClear%     '������
1369     '
1370     '�G���[���
1371     If MJudge% < MOK% Then
1372     '
1373 '�c���Ă���������ł͎g�p���Ȃ����x��
1374 *RETRY_ERR_WRITE
1375         M_20# = MClear%     '������
1376         '�G���[�����L�q
1377         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1378         'GOT KEY���͑҂�
1379         MKeyNumber = fnKEY_WAIT()
1380         '
1381         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1382             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1383            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1384             Break
1385         '
1386         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1387             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1388             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1389         '
1390         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1391             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1392             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1393         '
1394         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1395             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1396            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1397             Break
1398         '
1399         EndIf
1400         '
1401         If M_20# = MClear% Then *RETRY_ERR_WRITE
1402         '
1403     EndIf
1404     '
1405     If M_20# = MContinue% Then *RETRY_PIASWRITE
1406     '
1407     fnPiasWrite = 1
1408     Exit Function
1409 FEnd
1410 '
1411 '��fnPCBNumberCheck
1412 ''' <summary>
1413 ''' Pias ��ԍ��ƍ��v��
1414 ''' </summary>
1415 '''<param name="%"></param>
1416 '''<param name="%"></param>
1417 '''<returns></returns>
1418 ''' <remarks>
1419 ''' Date   : 2021/07/07 : M.Hayakawa
1420 ''' </remarks>'
1421 Function M% fnPCBNumberCheck
1422       fnPCBNumberCheck = 0
1423     '
1424 *RETRY_PCBCHECK
1425     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1426     'Pias�֊�ƍ��J�n M310 -> ON
1427     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1428     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1429     '
1430     MJudge% = MNG%
1431     '
1432     For MStaNo = 0 To 5
1433         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1434             MJudge% = MOK%
1435             fnAutoScreenComment(96)  'AUTO���
1436             MStaNo = 5
1437             Break
1438         '
1439         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1440             MJudge% = MNG%
1441             fnAutoScreenComment(97)  'AUTO���
1442             MCommentD1001 = 37
1443             MCommentD1002 = 25
1444             MStaNo = 5
1445             Break
1446         '
1447         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1448             MJudge% = MNG%
1449             fnAutoScreenComment(98)  'AUTO���
1450             MCommentD1001 = 38
1451             MCommentD1002 = 25
1452             MStaNo = 5
1453             Break
1454         '
1455         ElseIf M_In(11580) = 1 Then                         'time out
1456             MJudge% = MNG%
1457             fnAutoScreenComment(99)  'AUTO���
1458             MCommentD1001 = 39
1459             MCommentD1002 = 25
1460             MStaNo = 5
1461             Break
1462         '
1463         Else
1464             MJudge% = MNG%
1465            MCommentD1001 = 41
1466            MCommentD1002 = 25
1467         '
1468         EndIf
1469         '
1470     Next MStaNo
1471     '
1472     'Pias�֊�ƍ��J�n M310 -> OfF
1473     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1474     '
1475     '
1476     '�ʉߗ���NG �H�������̏ꍇ
1477     If MJudge% = MPass% Then
1478         M_20# = MPass%
1479     EndIf
1480     '
1481    M_20# = MClear%     '������
1482     '
1483     '�G���[���
1484     If MJudge% < MOK% Then
1485     '
1486 '�c���Ă���������ł͎g�p���Ȃ����x��
1487 *RETRY_ERR_PCBNUMBER
1488         M_20# = MClear%     '������
1489         '�G���[�����L�q
1490         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1491         'GOT KEY���͑҂�
1492         MKeyNumber = fnKEY_WAIT()
1493         '
1494         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1495             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1496             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1497             Break
1498         '
1499         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1500             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1501             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1502         '
1503         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1504             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1505             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1506         '
1507         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1508             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1509             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1510             Break
1511         '
1512         EndIf
1513         '
1514         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1515         '
1516     EndIf
1517     '
1518     If M_20# = MContinue% Then *RETRY_PCBCHECK
1519     Exit Function
1520 FEnd
1521 '
1522 '��ScrewTight
1523 ''' <summary>
1524 ''' �˂����߂��s��(S�^�C�g)
1525 ''' </summary>
1526 '''<param name="PScrewPos()">
1527 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1528 '''             PScrewPos(2)    �F�˂����߉��_
1529 '''             PScrewPos(10)   �F�˂����ߏI������
1530 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
1531 '''             1:6mm S�^�C�g��l�W
1532 '''             2:8mm P�^�C�g
1533 '''             3:6mm S�^�C�g���l�W
1534 '''             4:13mm S�^�C�g
1535 '''             5:6mm M�l�W
1536 '''</param>
1537 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
1538 '''<returns>����
1539 '''         0=�ُ�I���A1=����I��
1540 '''</returns>
1541 ''' <remarks>
1542 ''' Date   : 2021/07/07 : M.Hayakawa
1543 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
1544 ''' </remarks>'
1545 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
1546     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1547     ScrewTight = 0
1548     MOKNGFlg = 0
1549     Ovrd 100
1550     Mov PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
1551     Fine 0.05 , P
1552     Ovrd MOvrdA%
1553     ' �����ݒ�
1554     Accel 100, 10
1555     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
1556     Mvs PScrewPosition(2)
1557     ' �����������ɖ߂�
1558     Accel
1559     ' ����Ovrd�ݒ�
1560 '    Ovrd MOvrdA%
1561     Ovrd 100
1562     ' Spd�ݒ�
1563 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1564     Spd MFeedSpd
1565     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
1566     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1567     Select MScrewType%
1568         Case 1
1569             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1570             ProgramBankSet(1,1)
1571             Break
1572         Case 2
1573             ' P�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
1574             ProgramBankSet(3,1)
1575             Break
1576         Case 3
1577             ' S�^�C�g���F�v���O����1�A�o���N1�ɐݒ�
1578             ProgramBankSet(1,1)
1579             Break
1580         Case 4
1581             ' S�^�C�g13mm�F�v���O����1�A�o���N1�ɐݒ�
1582             ProgramBankSet(1,1)
1583             Break
1584         Case 5
1585             ' M�l�W�F�v���O����1�A�o���N1�ɐݒ�
1586             ProgramBankSet(1,1)
1587             Break
1588         Case 6
1589             ' S�^�C�g�F�v���O����1�A�o���N4�ɐݒ�
1590             ProgramBankSet(1,4)
1591             Break
1592         Default
1593             ' �v���O����1�A�o���N�Ȃ��ݒ�
1594             ProgramBankSet(0,0)
1595             Break
1596     End Select
1597 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1598      '�h���C�o�[ON�@CW
1599     M_Out(12241)=1
1600     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1601     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
1602     Dly 0.1
1603     Fine 0 , P
1604     Spd M_NSpd
1605     '
1606     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
1607         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1608         Dly 0.1
1609        ' �v���O�����E�o���N����
1610         ProgramBankSet(0,0)
1611         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1612         Mvs PScrewPosition(10),-80
1613         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1614         M_Out(12249)=1 Dly 0.3
1615         MOKNGFlg = -1
1616         ScrewTight = 0
1617     Else
1618          '�h���C�o�[OFF�@CW
1619         M_Out(12241)=0
1620 '        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
1621 '        Select MScrewType%
1622 '            Case 1
1623 '                ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1624 '                ProgramBankSet(1,3)
1625 '                Break
1626 '            Case 2
1627 '                ' P�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
1628 '                ProgramBankSet(3,3)
1629 '                Break
1630 '            Case 3
1631 '                ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
1632 '                ProgramBankSet(1,3)
1633 '                Break
1634 '            Case 4
1635 '                ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
1636 '                ProgramBankSet(1,3)
1637 '                Break
1638 '            Case 5
1639 '                ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
1640 '                ProgramBankSet(1,3)
1641 '                Break
1642 '            Default
1643 '                ' �v���O����1�A�o���N�Ȃ��ݒ�
1644 '                ProgramBankSet(0,0)
1645 '                Break
1646 '        End Select
1647 '         '�h���C�o�[ON�@CW
1648 '        Mvs PScrewPosition(10)
1649 '        M_Out(12241)=1
1650 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1651 '
1652          '�h���C�o�[OFF�@CW
1653         M_Out(12241)=0
1654        ' �v���O�����E�o���N����
1655         ProgramBankSet(0,0)
1656         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1657         M_Out(12249)=1 Dly 0.3
1658     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1659         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1660        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1661         'Mvs PScrewPosition(10),-80
1662         ScrewTight = 1
1663     EndIf
1664 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1665 '    Ovrd 10
1666 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1667     Ovrd 100
1668     Exit Function
1669 FEnd
1670 '
1671 '��ScrewGet
1672 ''' <summary>
1673 ''' �˂������@����˂��𓾂�
1674 ''' </summary>
1675 '''<param name="%">
1676 '''         PScrewPos(1)    �F�˂�������̂˂����
1677 '''         PScrewPos(2)    �F�˂���������_
1678 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1679 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1680 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1681 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1682 '''</param>
1683 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1684 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1685 '''<returns>����
1686 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1687 '''</returns>
1688 ''' <remarks>
1689 ''' Date   : 2021/07/07 : M.Hayakawa
1690 ''' </remarks>
1691 '''<update>
1692 '''Date    : 2021/11/15 : ����
1693 '''</update>
1694 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1695     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
1696     ScrewGet = 0
1697     MScrewJudge% = 0
1698     '�˂������평������G���[�`�F�b�N
1699 ' ���b��폜
1700     'Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
1701     For MCnt% = 0 To MFinCnt%
1702         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1703         If MRtn = 0 Then
1704             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1705             ScrewGet = -1
1706             MScrewJudge% = 2
1707         EndIf
1708         Ovrd 100
1709         If FeederScrewSensor% <> 0 Then
1710             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1711                 'Ovrd 30
1712                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1713                 'NG�Ƃ��Ă����̊֐����甲����
1714                 ScrewGet = -2
1715                 MScrewJudge% = 3
1716             EndIf
1717         EndIf
1718         Ovrd 100
1719         Spd M_NSpd
1720         If MScrewJudge% = 0 Then
1721     '        ScrewGet = 0
1722             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1723             MScrewCnt% = 0
1724             MFinCnt% = 2
1725             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
1726             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1727             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1728             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1729             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1730             'Mvs PScrewPosition(10), 1.2
1731             '�r�b�g��]
1732             M_Out(Y60_Driver)=1
1733             Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
1734             M_Timer(4) = 0
1735             MloopFlg = 0
1736             MCntTime& = 0
1737             While MloopFlg = 0
1738                 MCrtTime& = M_Timer(4)
1739                 If MCrtTime& >= 180 Then
1740                     MloopFlg = 1
1741                 EndIf
1742             WEnd
1743             M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1744             '�z���m�F
1745             MRtn = 0
1746             MRtn = frInCheck(11271, 1, MSETTIMEOUT01&)
1747             '
1748             JOvrd M_NJovrd
1749             Spd M_NSpd
1750             '�l�W�z���m�F�ʒu�ړ�
1751             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1752             Mvs PScrewPosition(10), -30  ' �l�W�z���m�F�ʒu
1753            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1754             '�r�b�g��]��~
1755             M_Out(Y60_Driver)=0
1756             '
1757             '1�b�ԃl�W�z���m�F �n�߂�臒l
1758 '            If MRtn = 1 Then           �@'�V�[�P���X�ύX�ɂ��R�����g�A�E�g(5/13����)
1759                 MRtn = frInCheck(11272, 1, MSETTIMEOUT03&)
1760 '            EndIf                      �@'�V�[�P���X�ύX�ɂ��R�����g�A�E�g(5/13����)
1761             'MRtn = 0'�����G���[
1762             '�z���G���[�̏ꍇ
1763             '�l�W���˂����Y�ɖ߂�
1764             If MRtn = 0 Then
1765                 Ovrd 30      '2����5�ɕύX
1766                 '�r�b�g��]��~
1767                 M_Out(Y60_Driver)=0
1768                 '�l�W�����@���
1769                 Mvs PScrewPosition(1)
1770                 '�X�ɏ��
1771                 Mov PScrewPosition(1), -140
1772                 '�l�W�̂Ĉʒu
1773                 MRtn = FnCtlValue2(4)          '�z���G���[���{�P  2022/04/28 �n��
1774                 Mov PScrewPosition(9)
1775                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1776                 '�z��OFF
1777                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1778                 Dly 0.2
1779                 '�j��ON
1780                 M_Out(Y6B_VB1)=1 '�^��j��ON
1781                 '�r�b�g��]
1782                 M_Out(Y61_Driver)=1
1783                 Dly 0.5
1784                 '                '
1785                 Ovrd 100
1786                 JOvrd M_NJovrd
1787                 Spd M_NSpd
1788                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1789                 Mov PScrewPosition(9), 10
1790                 Mov PScrewPosition(9)
1791                 Dly 0.1
1792                 Mov PScrewPosition(9), 10
1793                 Mov PScrewPosition(9)
1794                 '
1795                 '�l�W�����҂�
1796                 Wait M_In(11272) = 0
1797                 '�r�b�g��]��~
1798                 M_Out(Y61_Driver)=0
1799                 Dly 0.1
1800                 '�j��OFF
1801                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1802                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1803                 Mov PScrewPosition(1), -140
1804                 Ovrd 100
1805                 Spd M_NSpd
1806                 '�l�W�����@���
1807                 Mvs PScrewPosition(1)
1808 '                '
1809                 ScrewGet = -3
1810                 If MCnt% = MFinCnt% Then
1811                     MScrewJudge% = 4
1812                     Mov PScrewPosition(2)
1813                     Break
1814                 EndIf
1815                 Break
1816 '                '
1817             Else
1818                 MCnt% = MFinCnt%
1819                 ScrewGet = 1
1820             EndIf
1821         Else
1822             MCnt% =MFinCnt%
1823         EndIf
1824     Next  MCnt%
1825         '
1826 '    If MScrewJudge% = 0 Then
1827 '        Ovrd 100
1828 '        Spd M_NSpd
1829 '        PScrewPosition(1)
1830 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1831 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1832 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1833 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1834 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
1835 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
1836 '        'Mov PScrewPosition(2)
1837 '        '������x�z���m�F�@���̍ŏI臒l
1838 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1839 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1840 '            MScrewJudge% = 4
1841 '            ScrewGet = -3
1842 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1843 '            MScrewJudge% = 1
1844 '            ScrewGet = 1
1845 '        EndIf
1846 '        Break
1847 '    EndIf
1848     '
1849 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1850     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
1851     '
1852     Select MScrewJudge%
1853 '        Case 0
1854 ''            fErrorProcess(11,162,163,0) '�ُ�I��
1855 '            MCommentD1001 = 162
1856 '            MCommentD1002 = 96
1857 '            Break
1858         Case 2
1859 '            fErrorProcess(11,63,161,0) '����NG
1860             MCommentD1001 = 63
1861             MCommentD1002 = 96
1862             Break
1863         Case 3
1864 '            fErrorProcess(11,160,164,0) '�닟��
1865             MCommentD1001 = 237
1866             MCommentD1002 = 96
1867             Break
1868         Case 4
1869 '            fErrorProcess(11,94,95,0) '�z��NG
1870             MCommentD1001 = 94
1871             MCommentD1002 = 95
1872             Break
1873     End Select
1874     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1875     '
1876     Select M_20#
1877         Case MAbout%          '��~�������ꂽ�ꍇ
1878             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
1879             Mov PInitialPosition
1880             Break
1881         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1882             Break
1883         Case MNext%           '�p���������ꂽ�ꍇ
1884             M_20# = MClear%     '������
1885             Break
1886         Case MNgProcess%      'NG�������ꂽ�ꍇ
1887             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
1888             Mov PInitialPosition
1889             Break
1890         End Select
1891 *End_ScrewGet
1892     Exit Function
1893 FEnd
1894 '
1895 '��ProgramBankSet
1896 ''' <summary>
1897 ''' �˂����߂��s��(P�^�C�g)
1898 ''' </summary>
1899 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1900 '''<param name="MBankNo">�o���N�ԍ�</param>
1901 '''</returns>
1902 ''' <remarks>
1903 ''' Date   : 2021/10/05 : M.Hayakawa
1904 ''' </remarks>'
1905 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1906 '
1907     MLocalPrgNo% = (MProgramNo% - 1) * 32
1908     MLocalBankNo% = MBankNo% * 4
1909 '
1910     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1911         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1912     Else
1913         MLocalOutNo% = 0
1914     EndIf
1915 '
1916     M_Out8(12240) = MLocalOutNo%
1917     Dly 0.1
1918     Exit Function
1919 FEnd
1920 '
1921 '��fnKEY_WAIT()
1922 ''' <summary>
1923 ''' GOT����̃L�[���͑҂�
1924 ''' </summary>
1925 '''<returns>1�F��~    2�F����
1926 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1927 '''         5�FNG
1928 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1929 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1930 '''</returns>
1931 ''' <remarks>
1932 ''' Date   : 2021/07/07 : M.Hayakawa
1933 ''' </remarks>'
1934 Function M% fnKEY_WAIT()
1935     fnKEY_WAIT = 0
1936     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1937     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1938     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1939     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1940     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1941     Dly 0.2
1942     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1943     MLocalLoopFlg=1
1944     While MLocalLoopFlg=1
1945         If M_In(11345) = 1 Then         '��~   M5345
1946             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1947             fnKEY_WAIT = 1
1948             MLocalLoopFlg=-1
1949             Break
1950         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1951             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1952             fnKEY_WAIT = 2
1953             MLocalLoopFlg=-1
1954             Break
1955         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1956             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1957             fnKEY_WAIT = 3
1958             MLocalLoopFlg=-1
1959             Break
1960         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1961             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1962             fnKEY_WAIT = 4
1963             MLocalLoopFlg=-1
1964             Break
1965         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1966             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1967             fnKEY_WAIT = 5
1968             MLocalLoopFlg=-1
1969             Break
1970             '
1971         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1972             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1973             fnKEY_WAIT = MRobotInit1%
1974             MLocalLoopFlg=-1
1975             Break
1976             '
1977         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1978             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1979             fnKEY_WAIT = MRobotInit2%
1980             MLocalLoopFlg=-1
1981             Break
1982             '
1983         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1984             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1985             fnKEY_WAIT = MRobotInit3%
1986             MLocalLoopFlg=-1
1987             Break
1988             '
1989         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1990             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1991             fnKEY_WAIT = MRobotInit4%
1992             MLocalLoopFlg=-1
1993             Break
1994             '
1995         Else
1996         EndIf
1997     WEnd
1998     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1999     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2000     Exit Function
2001 FEnd
2002 '
2003 '�� fnAUTO_CTL
2004 ''' <summary>
2005 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2006 ''' </summary>
2007 ''' <remarks>
2008 ''' Date   : 2021/07/07 : M.Hayakawa
2009 ''' </remarks>
2010 Function M% fnAUTO_CTL
2011     fnAUTO_CTL = 0
2012     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2013     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2014     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2015     '
2016     If M_Svo=0 Then             '�T�[�{ON�m�F
2017         Servo On
2018     EndIf
2019     Wait M_Svo=1
2020     Exit Function
2021 FEnd
2022 '
2023 '�� fnWindScreenOpen
2024 ''' <summary>
2025 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2026 ''' </summary>
2027 '''<param name="%"></param>
2028 '''<param name="%"></param>
2029 '''<param name="%"></param>
2030 '''<param name="%"></param>
2031 ''' <remarks>
2032 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2033 ''' MWindReSet = 0     ��ʔ�\��
2034 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2035 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2036 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2037 ''' Date   : 2021/07/07 : M.Hayakawa
2038 ''' </remarks>
2039 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2040     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2041         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2042     EndIf
2043     '
2044     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2045         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2046     EndIf
2047     '
2048     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2049        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2050     EndIf
2051     '
2052     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2053     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2054     Dly 0.5
2055     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2056     Exit Function
2057 FEnd
2058 '
2059 '��FnCtlValue2
2060 ''' <summary>
2061 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2062 ''' </summary>
2063 ''' <param name="MCtlNo%"></param>
2064 ''' <remarks>
2065 ''' Date : 2022/04/28 �n��
2066 ''' </remarks>
2067 '''
2068 '''  1�F������       �{�P
2069 '''  2�F�g���n�j��   �{�P
2070 '''  3�F�g���m�f��   �{�P (���g�p)
2071 '''  4�F�z���G���[�� �{�P
2072 ''' 99�F�Ǐ��J�n�M�� OFF
2073 '''
2074 Function M% FnCtlValue2(ByVal MCtlNo%)
2075     FnCtlValue2 = 1
2076     Select MCtlNo%
2077         Case 1        '�������{�P
2078             M_Out(12569) = 0             '�����݊J�n�M��OFF
2079             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2080             MInputQty = M_In16(11600)    '��������M
2081             MInputQty = MInputQty + 1    '�������{�P
2082             M_Out16(12592) = MInputQty   '���������M
2083             M_Out(12569) = 1             '�����݊J�n�M��ON
2084             Break
2085             '
2086         Case 2        '�g���n�j���{�P
2087             M_Out(12569) = 0             '�����݊J�n�M��OFF
2088             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2089             MAssyOkQty = M_In16(11616)   '�g��OK����M
2090             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2091             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2092             M_Out(12569) = 1             '�����݊J�n�M��ON
2093             Break
2094             '
2095         Case 4        '�z���G���[���{�P
2096             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2097             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2098             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2099             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2100             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2101             M_Out(12569) = 1                       '�����݊J�n�M��ON
2102             Break
2103             '
2104         Case 99        '�Ǐ��J�n�M��OFF
2105             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2106             M_Out(12569) = 0        '�����݊J�n�M��OFF
2107             Break
2108             '
2109     End Select
2110     Exit Function
2111 FEnd
2112 '
2113 '
2114 '��FnScreEroorCord
2115 ''' �d���h���C�o�[�̃G���[�R�[�h���܂߂��R�����g���o���ׂ̃R�����g�ԍ��̍쐬
2116 ''' �V�K�쐬�F2022/05/23 : �n��
2117 '''
2118 Function M% FnScreEroorCord()
2119     MScrewErrorCord% = 0
2120     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2121     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2122     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2123     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2124     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2125     MScrewErrorCord% = MScrewErrorCord% * 10
2126     MScrewErrorCord% = MScrewErrorCord% + 500
2127     FnScreEroorCord = MScrewErrorCord%
2128     Exit Function
2129 FEnd
2130 '
2131 '
2132 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2133 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2134 '-------------------------------------------------------------------------------
2135 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2136 '   ����
2137 '       PInspPos()      �F�����ʒu
2138 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2139 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2140 '       MInspCnt%       �F�����ʒu��
2141 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2142 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2143 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2144 '   �߂�l�F����
2145 '       0=�ُ�I���A1=����I��
2146 '
2147 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2148 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2149 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2150 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2151 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2152 '-------------------------------------------------------------------------------
2153     '----- �����ݒ� -----
2154     Cnt 0                                                           '�ړ�����������(�����l=0)
2155     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2156 '    Cnt 1,0.1,0.1
2157     '�ϐ��錾�E������
2158     Def Inte MNum                                                   '�����ԍ�(������1�`)
2159     MNum% = 1                                                       '�����ԍ������l�ݒ�
2160     Def Inte MEndFlg                                                '�����I���t���O
2161     MEndFlg% = 0
2162     '
2163     '����G�ԍ��ݒ�v���E�������s�v��off
2164     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2165     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2166     '�G���[�ԍ��N���A
2167     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2168     M_Out16(MOUT_InspErrNum) = MInspErrNum
2169     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2170     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2171     '
2172     'Insight Ready check?
2173     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2174         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2175         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2176         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2177         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2178         Exit Function
2179     EndIf
2180     '
2181     '�����ʒu���m�F
2182     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2183         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2184         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2185         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2186         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2187         Exit Function
2188     EndIf
2189     '
2190     '
2191     '
2192     '----- ���C������ -----
2193     '�ݒ肳�ꂽ�����ʒu�����̌������s
2194     While( MEndFlg% = 0 )
2195         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2196         MSetGrNumRetryExitFlg = 0
2197         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2198         While( MSetGrNumRetryExitFlg = 0 )
2199         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2200             '
2201             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2202             '
2203             '----- �����O���[�v�ԍ��ݒ� -----
2204             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2205             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2206             '
2207             '�����ʒu�ֈړ��E�ړ������҂�
2208             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2209             Mvs PInspPos( MNum% )                                       '�ړ�
2210             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2211             Dly 0.05                                                    '�ړ�������Delay
2212             '
2213             '�����O���[�v�ԍ��ݒ�I���m�F
2214             M_Timer(1) = 0
2215             MExitFlg = 0
2216             While( MExitFlg = 0 )
2217                 '����G�ݒ萳��I��?
2218                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2219                     MExitFlg = 1
2220                 '
2221                 '����G�ݒ�ُ�I��?
2222                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2223                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2224                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2225                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2226                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2227                     EndIf
2228                     MExitFlg = 1
2229                 '
2230                 'timeout�`�F�b�N
2231                 ElseIf 1000 < M_Timer(1) Then
2232                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2233                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2234                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2235                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2236                     EndIf
2237                     MExitFlg = 1
2238                 EndIf
2239             WEnd
2240             '
2241             '����G�ԍ��ݒ�v��off
2242             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2243             '
2244             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2245             'NG�Ȃ���Δ�����
2246             If MCurrentStepErr = 0 Then
2247                 MSetGrNumRetryExitFlg = 1
2248             Else
2249                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2250                 If MSetGrNumRetryCnt = 0 Then
2251                     MSetGrNumRetryExitFlg = 1
2252                 Else
2253                     'Retry�ց@���̑O��Delay
2254                     Dly 0.5
2255                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2256                 EndIf
2257             EndIf
2258             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2259             '
2260         WEnd
2261         '
2262         '
2263         '
2264         '----- �������s -----
2265         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2266             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2267                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2268                 MInspRetryExitFlg = 0
2269                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2270                 While( MInspRetryExitFlg = 0 )
2271                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2272                     '
2273                     '���������m�F
2274                     MRetryCnt = MRetryCnt - 1
2275                     M_Timer(1) = 0
2276                     MExitFlg = 0
2277                     While( MExitFlg = 0 )
2278                     '���������҂�
2279                         '����OK�I��?
2280                         If M_In( MIN_IS_InspOK% ) = 1  Then
2281                             MJudgeOKFlg = 1                         '����OK�t���OON
2282                             MExitFlg = 1
2283                         '
2284                         '����NG�I��?
2285                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2286                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2287                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2288                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2289                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2290                                 EndIf
2291                             EndIf
2292                             MExitFlg = 1
2293                         '
2294                         '�����ُ�I��(IS timeout)?
2295                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2296                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2297                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2298                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2299                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2300                                 EndIf
2301                             EndIf
2302                             MExitFlg = 1
2303                         '
2304                         'timeout�`�F�b�N
2305                         ElseIf 3000 < M_Timer(1) Then
2306                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2307                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2308                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2309                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2310                                 EndIf
2311                             EndIf
2312                             MExitFlg = 1
2313                         EndIf
2314                     WEnd
2315                     '
2316                     '�����J�n�v��off
2317                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2318                     '
2319                     'OK�Ȃ甲����
2320                     If MJudgeOKFlg = 1 Then
2321                         MInspRetryExitFlg = 1
2322                     Else
2323                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2324                         If MRetryCnt = 0 Then
2325                             MInspRetryExitFlg = 1
2326                         Else
2327                             'Retry�ց@���̑O��Delay
2328                             Dly 0.3
2329                         EndIf
2330                     EndIf
2331                     '
2332                 WEnd
2333             EndIf
2334         EndIf
2335         '
2336         '
2337         '
2338         MNum% = MNum% + 1                                           '����Step+1
2339         '�����I���m�F�@�����I���t���O�Z�b�g
2340         If (MInspCnt% < MNum% ) Then
2341             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2342         EndIf
2343         'NG���������s������
2344         If MInspErrNum <> 0 Then                                    'NG����?
2345             If MNgContinue% <> 1 Then                               'NG���s?
2346                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2347             EndIf
2348         EndIf
2349     WEnd
2350     '
2351     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2352     If 0 < MZAxis% Then
2353         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2354         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2355         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2356         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2357     EndIf
2358     '
2359     '�߂�l�ݒ�
2360     If MInspErrNum = 0 Then
2361         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2362     Else
2363         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2364         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2365         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2366     EndIf
2367     Fine 0 , P
2368     Exit Function
2369 FEnd
2370 '
2371 '��fnAutoScreenComment
2372 ''' <summary>
2373 ''' ���C����ʂ̓���󋵕\��
2374 ''' �R�����gD1005�̐ݒ�
2375 ''' </summary>
2376 '''<param name="McommentD1005%">�R�����gID</param>
2377 ''' <remarks>
2378 ''' Date   : 2021/07/07 : M.Hayakawa
2379 ''' </remarks>
2380 Function fnAutoScreenComment(ByVal McommentD1005%)
2381     M_Out16(12576) = McommentD1005%
2382     Exit Function
2383 FEnd
2384 '
2385 '��fnRoboPosChk
2386 ''' <summary>
2387 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2388 ''' </summary>
2389 '''<param name="MINNumber%">���͔ԍ�</param>
2390 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2391 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2392 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2393 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2394 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2395 ''' <remarks>
2396 ''' Date   : 2021/07/07 : M.Hayakawa
2397 ''' </remarks>
2398 Function M% fnRoboPosChk
2399     fnRoboPosChk = 0
2400     MRet = fnStepRead()
2401     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2402     '�E�B���h��ʐ؊���
2403     If MRBTOpeGroupNo > 5 Then
2404         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2405         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2406         Dly 0.2
2407         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2408         Dly 1.5
2409         '
2410         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2411         '
2412         MLoopFlg% = 1
2413         While MLoopFlg% = 1
2414             '
2415             '
2416             MKeyNumber% = fnKEY_WAIT()
2417             Select MKeyNumber%
2418                 Case Is = MAbout%       '��~
2419                     M_20# = MAbout%
2420                     MLoopFlg% = -1
2421                     Break
2422                 Case Is = MNext%        '����
2423                     'MLoopFlg% = -1
2424                     Break
2425                 Case Is = MContinue%    '�p��
2426                     M_20# = MContinue%
2427                     MLoopFlg% = -1
2428                     Break
2429                 Default
2430                     Break
2431             End Select
2432         WEnd
2433     EndIf
2434     '
2435     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2436         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2437         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2438         Select MRBTOpeGroupNo
2439             Case Is = 5                          '�������Ȃ�
2440                 Break
2441             Case Is = 10                         '�����ʒu�֖߂�
2442                 'Mov PTEST001
2443                 Break
2444             Case Is = 15                         '�����ʒu�֖߂�
2445                 'Mov PTEST002
2446                 Dly 0.5
2447                 'Mov PTEST001
2448                 Dly 0.5
2449                 Break
2450             Default
2451                 Break
2452         End Select
2453         '
2454         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2455         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2456         MRBTOpeGroupNo = 5
2457         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2458         Dly 1.0
2459         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2460         fnRoboPosChk = 1                        '�����ʒu������s
2461         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2462     EndIf
2463     Exit Function
2464 FEnd
2465 '
2466 '��frInCheck
2467 ''' <summary>
2468 ''' �Z���T�[IN�`�F�b�N
2469 ''' </summary>
2470 '''<param name="MINNumber%">���͔ԍ�</param>
2471 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2472 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2473 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2474 ''' <remarks>
2475 ''' Date   : 2021/07/07 : M.Hayakawa
2476 ''' </remarks>
2477 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2478     M_Timer(4) = 0
2479     MloopFlg = 0
2480     While MloopFlg = 0
2481         MCrtTime& = M_Timer(4)
2482         If M_In(MINNumber%) = MCMPFLG% Then
2483             MloopFlg = 1
2484             frInCheck = 1
2485         ElseIf MCrtTime& > MTimeCnt& Then
2486             MloopFlg = 1
2487             frInCheck = 0
2488         EndIf
2489     WEnd
2490     Exit Function
2491 FEnd
2492 '-----------------------------------------------
2493 '
2494 '�˂����ߋ@�ʐM�m�F
2495 '
2496 '-----------------------------------------------
2497 Function M% fScewTcomChk
2498     fScewTcomChk = 0
2499     '�ʐM�m�F���M
2500     M_Out(MOUT_ScwT_ComChk%) = MOn%
2501     '�ʐM�m�F��M�ҋ@
2502     Wait M_In(MIN_ScwT_comOK%) = MOn%
2503     '�ʐM�m�F���M�I��
2504     M_Out(MOUT_ScwT_ComChk%) = MOff%
2505     Exit Function
2506 FEnd
2507 '
2508 '
2509 '-----------------------------------------------
2510 '
2511 '�˂����ߊJ�n���M
2512 '
2513 '-----------------------------------------------
2514 Function M% fScewTStart
2515     fScewTStart = 0
2516     '�˂����ߊJ�n�ҋ@����M
2517     Wait M_In(MIN_ScwT_STRec%) = MOn%
2518     Dly 0.1
2519     '�˂����ߊJ�n��M�𑗐M
2520     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
2521     Exit Function
2522 FEnd
2523 '
2524 '
2525 '-----------------------------------------------
2526 '
2527 '�˂����ߊ�����M
2528 '
2529 '-----------------------------------------------
2530 Function M% fScewTFinish
2531     fScewTFinish = 0
2532     '�˂����ߊ����ҋ@����M
2533     Wait M_In(MIN_ScwT_Fin%) = MOn%
2534     Dly 0.1
2535     '�˂����ߊ�����M�𑗐M
2536     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
2537     Exit Function
2538 FEnd
2539 '
2540 '
2541 '-----------------------------------------------
2542 '
2543 '����xx��~��M
2544 '
2545 '-----------------------------------------------
2546 Function M% fScewTCaseStop(ByVal MCase%())
2547     fScewTCaseStop = 0
2548     '����xx��~����M
2549     Wait M_In(MCase%(1)) = MOn%
2550     Dly 0.1
2551     '����xx��~��M�𑗐M
2552     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
2553     Exit Function
2554 FEnd
2555 '
2556 '-----------------------------------------------
2557 '
2558 '�ĊJ�n��M
2559 '
2560 '-----------------------------------------------
2561 Function M% fScewTReStart()
2562     fScewTReStart = 0
2563     '�ĊJ�n����M
2564     Wait M_In(MIN_ScwT_ReST%) = MOn%
2565     Dly 0.1
2566     '�ĊJ�n��M�𑗐M
2567     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
2568     Exit Function
2569 FEnd
2570 '
2571 '��fErrorProcess
2572 '<summary>
2573 '�G���[����
2574 '</summary>
2575 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2576 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2577 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2578 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2579 '<make>
2580 '2021/11/5 �����V��
2581 '</make>
2582 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2583     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2584     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2585     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2586     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2587 *RETRY_ERR_PROCESS
2588      M_20# = MClear%     '������
2589 '        '�G���[�����L�q
2590         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2591 '        'GOT KEY���͑҂�
2592         MKeyNumber = fnKEY_WAIT()
2593 '        '
2594         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2595             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2596 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2597             Break
2598          '
2599         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2600             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2601 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2602         '
2603         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2604             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2605 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2606          '
2607         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2608             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2609 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2610             Break
2611         '
2612         EndIf
2613         '
2614         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2615         fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2616     Exit Function
2617 FEnd
2618 '
2619 '��fnInitialZone
2620 ''' <summary>
2621 ''' ���݈ʒu������ɑҔ����A�����ʒu�ɖ߂�
2622 ''' </summary>
2623 ''' <param name="posNum%">�ړ���̃|�W�V�����ԍ�</param>
2624 ''' <remarks>
2625 ''' Date : 2021/12/2 : M.Hayakawa
2626 ''' Update:2022/06/2 : M.Hayakawa ���H���̔���~���A�ɍ��킹�ĕύX
2627 ''' </remarks>
2628 Function fnInitialZone()
2629     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���]
2630 '
2631     Ovrd 5
2632 ' ���ޔ�
2633     PActive = P_Curr
2634     Pmove = PActive
2635 '
2636     If PActive.X > 580 Then
2637         Pmove.Z =380        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2638     Else
2639         Pmove.Z =500        '��L�ȊO��Z:500�܂Ŏ����グ
2640     EndIf
2641 '
2642     Mvs Pmove
2643     Mov PInitialPosition
2644 ' ���b�N���J��
2645     InitialState()
2646 ' ��U��~
2647     fErrorProcess(20,70,256,0)
2648     Exit Function
2649  FEnd
2650 '
2651 '��InitialState
2652 ''' <summary>
2653 ''' �n���h�A����������ʒu�ɂ���
2654 ''' </summary>
2655 ''' <returns>   0 : OK
2656 '''             1 : NG
2657 ''' </returns>
2658 ''' <remarks>
2659 ''' Date : 2021/12/2 : M.Hayakawa
2660 ''' </remarks>
2661 Function M% InitialState()
2662     InitialState = 0
2663     '
2664     '�ʒu���߉���
2665     M_Out(12264) = 0
2666     M_Out(12265)=1 Dly 0.3                  '�v�b�V������
2667     'Wait M_In(11276)=1                      '�v�b�V���ߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2668     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    '�v�b�V���ʒu�ߒ[���o(8/26����)
2669     If MRtn = 0 Then
2670         fErrorProcess(11,234,284,0)
2671         Select M_20#
2672             Case MAbout%                    '��~�������ꂽ�ꍇ
2673                 InitialState = 1
2674                 Break
2675             Case MNgProcess%
2676                 InitialState = 1
2677                 Break
2678             Case MContinue%                 '���g���C�������ꂽ�ꍇ
2679                 M_20# = MClear%
2680                 InitialState = 0
2681                 Break
2682             Case MNext%                     '���ւ������ꂽ�ꍇ
2683                 M_20# = MClear%
2684                 InitialState = 0
2685                 Break
2686         End Select
2687     EndIf
2688     *RETRY_POSITIONING_RESTORE
2689     '
2690     M_Out(12262) = 0
2691     M_Out(12263)=1 Dly 0.3                  '�ʒu���߉���
2692     'Wait M_In(11274)=1                      '�ʒu���ߖߒ[���o(�C���ɂ��R�����g�A�E�g(8/26����))
2693     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[���o(8/26����)
2694     If MRtn = 0 Then
2695         fErrorProcess(11,234,284,0)
2696         Select M_20#
2697             Case MAbout%                    '��~�������ꂽ�ꍇ
2698                 InitialState = 1
2699                 Break
2700             Case MNgProcess%
2701                 InitialState = 1
2702                 Break
2703             Case MContinue%                 '���g���C�������ꂽ�ꍇ
2704                 M_20# = MClear%
2705                 InitialState = 0
2706                 Break
2707             Case MNext%                     '���ւ������ꂽ�ꍇ
2708                 M_20# = MClear%
2709                 InitialState = 0
2710                 Break
2711         End Select
2712     EndIf
2713     Exit Function
2714 FEnd
2715 '
2716 '��fnTorqueCheck
2717 ''' <summary>
2718 ''' �g���N�`�F�b�N����p�̃��C��
2719 ''' </summary>
2720 ''' <remarks>
2721 ''' Date   : 2021/12/21 : H.AJI
2722 ''' </remarks>'
2723 Function M% fnTorqueCheck
2724     '�g���N�`�F�b�N�����M  �����n��~
2725     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2726     '
2727     fnTorqueCheck = 0
2728     Ovrd 20
2729     Mov PInitialPosition              '�����ʒu�ړ�
2730     Ovrd 100
2731     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2732     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2733     Dly 0.2
2734     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2735     '
2736     'M6340  �g���N�`�F�b�N��M
2737     'Dly 5.0
2738     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
2739     Dly 1.0
2740     M_Out(12340) = 0
2741     '
2742     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
2743     '
2744     MLoopFlg = 1
2745     While MLoopFlg = 1
2746         '
2747         Mov PInitialPosition              '�����ʒu�ړ�
2748         '
2749         MKeyNumber = fnKEY_WAIT()
2750         Select MKeyNumber
2751             Case Is = 1           '��~
2752                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
2753                 Dly 1.0
2754                 M_Out(12343) = 0
2755                 Ovrd 20
2756                 Mov PTicketRead_1
2757                 Ovrd 100
2758                 M_20# = 1
2759                 MLoopFlg = -1
2760                 Break
2761             Case Is = 2           '����
2762                 Break
2763             Case Is = 3           '�p��
2764                 Break
2765             Case Is = 4           '�g���N�`�F�b�N�J�n
2766                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
2767                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
2768                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2769                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2770                 MRet = fnMoveTorquePosi()
2771                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
2772                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2773                 Break
2774             Default
2775                 Break
2776         End Select
2777     WEnd
2778     '
2779     '�g���N�`�F�b�N����~���M
2780     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2781     '
2782     '���{�b�g�̈ʒu�����ɖ߂�
2783     '
2784     Exit Function
2785  FEnd
2786  '
2787 '
2788 '
2789 '---------------------------
2790 '
2791 '    ���C����ʂ̕\���A��\���ݒ�
2792 '         �R�����gD1001, D1002, D1003�̐ݒ�
2793 '           MWindReSet = 0     ��ʔ�\��
2794 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2795 '           MWindErrScr = 10    �G���[��� D1001, D1002
2796 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2797 '
2798 '---------------------------
2799 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2800     fnMainScreenOpen = 0
2801     '
2802    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2803         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2804     EndIf
2805     '
2806     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2807         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2808     EndIf
2809     '
2810     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2811         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2812     EndIf
2813     '
2814     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2815     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
2816     Dly 0.5
2817     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
2818     Exit Function
2819 FEnd
2820 '
2821 '��Main
2822 ''' <summary>
2823 ''' �g���N�`�F�b�N������
2824 ''' </summary>
2825 ''' <remarks>
2826 ''' Date   : 2021/12/21 : H.AJI
2827 ''' </remarks>'
2828 Function M% fnMoveTorquePosi
2829      fnMoveTorquePosi = 0
2830      Ovrd 50
2831      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
2832     '
2833     Spd M_NSpd
2834 '-------------      �h���C�o�[RST
2835     M_Out(12240)=0     '�h���C�o�[OFF CCW
2836     M_Out(12241)=0     '�h���C�o�[OFF CW
2837     M_Out(12242)=1     '�h���C�o�[���� C1
2838     M_Out(12243)=1     '�h���C�o�[���� C2
2839     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
2840 '---------------------------------------
2841 '[P-11]
2842 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
2843     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
2844     Dly 0.1
2845 '-----------------------
2846    'Cnt 0                           'Cnt����-2�@�I��
2847 '-----------------------
2848     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
2849     Dly 0.2
2850 '-----------------------
2851     ProgramBankSet(1,3)
2852     M_Out(12241)=0                   '�h���C�o�[OFF  CW
2853     'Dly 0.1
2854 '--------------------------------
2855     Ovrd 40
2856    'Dly 0.1
2857 '--------------------------------  �l�W���ߑ��x�ݒ�
2858     Spd 14                            '���C�h 100-40 100% :Spd 12
2859     Dly 0.1
2860 '--------------------------------
2861 '--------------------------------
2862 '---------------------------------�y�˂����ߓ���z
2863 '
2864     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
2865    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
2866     Dly 0.3                          '�������҂�
2867    M_Out(12241)=1                   '�h���C�o�[ON  CW
2868 '
2869     Wait M_In(11584)=1                '����/�G���[���o
2870     Dly 0.1
2871     Spd M_NSpd
2872    'Ovrd 20
2873     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
2874     Wait M_In(11257)=1                '�l�W����SC
2875 '---------------------------------
2876     Dly 0.1
2877     M_Out(12241)=0                    '�h���C�o�[OFF CW
2878     Dly 0.1
2879     M_Out(12242)=0                    '�h���C�o�[���� C1
2880     Dly 0.1
2881     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
2882     Dly 0.1
2883     M_Out(12245)=0                    '�v���O����2���� F1
2884 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
2885 '
2886     Mvs PTorqueCheck,-60                       '������mov ����ύX
2887     Dly 0.1
2888 '--------------------------------------------------------------
2889    'Ovrd 80
2890 '--------------------------------------------------------------
2891 '---------------------------------------
2892 '---------------------------------------
2893 '---------------------------------------�G���[���E����
2894    *LBL1
2895    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
2896    Mvs ,-100
2897    M_Out(12241)=0     '�h���C�o�[OFF CW
2898    Dly 0.1
2899    M_Out(12242)=0     '�h���C�o�[���� C1
2900    Dly 0.1
2901    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
2902    Dly 0.1
2903    M_Out(12245)=0     '�v���O�������� F1
2904 '---------------------------------------
2905 '---------------------------------------
2906 '-------------
2907    'Mov PInitPos19049
2908    Dly 0.1
2909 '
2910 '
2911     Exit Function
2912 FEnd
2913 '
2914 '��Main
2915 ''' <summary>
2916 ''' �g������p�̃��C��
2917 ''' </summary>
2918 ''' <remarks>
2919 ''' Date   : 2021/07/07 : M.Hayakawa
2920 ''' </remarks>'
2921 Function Main
2922     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
2923     '
2924     If M_Svo=0 Then
2925         Servo On
2926     EndIf
2927     Wait M_Svo=1
2928 '�g���X�^�[�g���t�����v���p���XON
2929     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2930 '�p�g���C�g����
2931     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
2932     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
2933     '
2934     M_20# = 0                                   'KEY���͏�����
2935     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
2936     MRet% = 0
2937 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
2938     PActive = P_Curr                    '���݈ʒu���擾
2939     MRecoveryPass% = 0
2940     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2941         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2942             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2943             MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
2944         EndIf
2945     EndIf
2946     EndIf
2947     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2948         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2949             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2950                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
2951             EndIf
2952         EndIf
2953     EndIf
2954     If MRecoveryPass% = 0 Then
2955         fnInitialZone()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
2956     EndIf
2957     '
2958     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2959         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2960 '�g���N�`�F�b�N
2961         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2962             MRet% = fnTorqueCheck()
2963             Break
2964         Else
2965 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
2966 '                MRtn = InspInit()               '�摜��������������
2967 '            EndIf
2968             '
2969            M_20# = MClear%                    '������
2970 '�g���J�n
2971             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2972                 MRet% = fnAssyStart()
2973             Else
2974                 M_20# = MPass%
2975             EndIf
2976 '�g���I�����t����
2977             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2978             Wait M_In(11572) = 1            '���t�擾����
2979             Dly 0.1
2980             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2981 '���t�^�[���j�b�g�ւ�OUT
2982             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2983             fnAutoScreenComment(89)         'AUTO��� �g����������
2984             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
2985 'OK/NG�t���O�o��
2986             If M_20# <= 0 Then
2987                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
2988             ElseIf M_20# = MPass% Then
2989                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
2990             EndIf
2991 'PIAS�ɑg������������
2992             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
2993                 If M_20# = MPass% Then
2994                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
2995                 Else
2996                     'KEY���͂�NG�̏ꍇ
2997                     If M_20# = MNgProcess% Then
2998                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
2999                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3000                         MRet% = fnPiasWrite(MNG%)
3001                        nAssyNgQty = nAssyNgQty + 1
3002                     EndIf
3003                     '
3004                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/07����)
3005                     If M_20# = MAssyOK% Then
3006                             '-----------------------
3007                             'D732 -> D2600 �R�s�[�v��
3008                             M_Out(12566) = 1
3009 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3010                             M_Out(12566) = 0
3011                             '
3012                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3013                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3014                             '��ԍ��ƍ�(PP�͖��g�p�j
3015 '                            MRet% = fnPCBNumberCheck()
3016                         Else
3017                             MRet% = 1
3018                         EndIf
3019                         '
3020                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3021                             If M_20# <> MAbout% Then
3022                                 '�H������OK��������
3023                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3024                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3025                                 MRet% = fnPiasWrite(MOK%)
3026                                 nAssyOkQty = 0
3027                                 nAssyOkQty = nAssyOkQty + 1
3028                             Else
3029                                 nAssyOkQty = nAssyOkQty + 1
3030                             EndIf
3031                         EndIf
3032                     EndIf
3033 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3034 '                    MRet% = fnPiasWrite(MOK%)
3035                 EndIf
3036             Else
3037                 nAssyOkQty = nAssyOkQty + 1
3038             EndIf
3039             '
3040             '�g���I�����t��������
3041             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3042             '�������A�g��OK���A�g��NG��������
3043 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3044             '
3045 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3046 '                '�摜�����I������
3047 '                MRtn = InspQuit()
3048 '            EndIf
3049         EndIf
3050         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3051     EndIf
3052 '�p�g���C�g����
3053     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3054     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3055 'GOT�\��
3056     fnAutoScreenComment(93)  'AUTO��� �H������
3057 FEnd
3058 End
3059 '
3060 '���܂��Ȃ��R�����g
3061 '��΍폜�����
3062 '
3063 '
3064 '
3065 '
3066 '
PInspPosition(1)=(+343.72,-16.25,+435.00,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
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
PTemp=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(+322.08,-175.00,+395.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(2)=(+322.08,-175.00,+336.25,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+322.08,-175.00,+330.25,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PGetScrewPos(1)=(+180.80,+240.77,+380.00,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(2)=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00,+0.00,+0.00)(7,0)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
PGetScrewPos(10)=(+180.80,+240.77,+339.80,-180.00,+0.00,-120.00,+0.00,+0.00)(7,0)
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
PActive=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
Pmove=(+603.00,-149.18,+380.00,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
PInitialPosition=(+250.00,+0.00,+450.00,+180.00,+0.00,+180.00)(7,0)
PScrewSoc1=(+300.80,-66.74,+330.15,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_0=(+300.80,-66.74,+336.15,-180.00,+0.00,+90.00)(7,0)
PScrewSoc1_1=(+300.80,-66.74,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2=(+322.31,-26.38,+330.39,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_0=(+322.31,-26.38,+336.39,-180.00,+0.00,+90.00)(7,0)
PScrewSoc2_1=(+322.31,-26.38,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3=(+382.42,-26.61,+330.49,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_0=(+382.42,-26.61,+336.49,-180.00,+0.00,+90.00)(7,0)
PScrewSoc3_1=(+382.42,-26.61,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4=(+392.51,-87.71,+329.90,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_0=(+392.51,-87.71,+335.90,-180.00,+0.00,+90.00)(7,0)
PScrewSoc4_1=(+392.51,-87.71,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5=(+371.80,-155.86,+330.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_0=(+371.80,-155.86,+336.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc5_1=(+371.80,-155.86,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6=(+322.08,-175.00,+330.25,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_0=(+322.08,-175.00,+336.25,-180.00,+0.00,+90.00)(7,0)
PScrewSoc6_1=(+322.08,-175.00,+395.00,-180.00,+0.00,+90.00)(7,0)
PScrewSupply=(+180.80,+240.77,+339.80,-180.00,+0.00,-120.00)(7,0)
PScrewSupply_1=(+180.80,+240.77,+380.00,-180.00,+0.00,-120.00)(7,0)
PScrewSupply_2=(+182.97,+239.67,+400.00,+180.00,+0.00,+180.00)(7,0)
PScrewSupply_9=(+78.32,+270.81,+429.99,-180.00,+0.00,-120.00)(7,0)
PSocCheck=(+325.25,-60.87,+444.00,+180.00,-0.01,-180.00)(7,0)
PSocCheck_1=(+325.25,-60.87,+470.00,+180.00,-0.01,-180.00)(7,0)
PSocGet=(+627.93,+104.72,+312.87,+179.69,+0.00,-179.37)(7,0)
PSocGet_1=(+627.93,+104.72,+340.00,+179.69,+0.00,-179.37)(7,0)
PSocGet_2=(+628.22,+107.24,+380.00,-179.93,+0.04,-178.10)(7,0)
PSocPcbRead=(+343.72,-16.25,+435.00,-180.00,+0.00,-180.00)(7,0)
PSocPcbRead_1=(+343.72,-16.25,+480.00,-180.00,+0.00,-180.00)(7,0)
PSocPress=(+393.39,+10.43,+354.20,+180.00,+0.00,-180.00)(7,0)
PSocPress_1=(+393.39,+10.43,+369.00,-180.00,+0.00,+180.00)(7,0)
PSocPress_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PSocSet=(+486.68,-100.68,+350.00,+179.65,+0.00,-179.18)(7,0)
PSocSet_1=(+486.68,-100.68,+361.91,+179.65,+0.00,-179.18)(7,0)
PSocSet_2=(+486.68,-100.68,+380.00,+179.65,+0.00,-179.18)(7,0)
PTicketRead=(+603.00,-149.18,+373.00,-179.99,+0.00,+90.00)(7,0)
PTicketRead_1=(+603.00,-149.18,+450.00,-179.99,+0.00,+90.00)(7,0)
PTorqueCheck=(+144.46,-240.78,+340.00,-179.99,-0.01,+90.02)(7,0)
PTorqueCheck_1=(+144.45,-240.80,+360.00,-179.99,+0.00,+90.01)(7,0)
