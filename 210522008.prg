1 ' ===================================
2 '
3 '  2100301001 STEP5 Assy1�v���O����
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
213 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
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
227 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
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
269 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u '���l12250����12248�֕ύX(8/5����)
270 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
271 MOUT_VB1%   =  12250    ' �A�[����[�@�l�W�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
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
330 MOUT_ScwT_ComChk% = 12816               '�ʐM�m�F���M
331 MOUT_ScwT_ST% = 12849                   '�˂����ߊJ�n�𑗐M
332 MOUT_ScwT_ReSTOK% = 12850               '�ĊJ�n��M�𑗐M
333 MOUT_ScwT_FinOK% = 12852                '�˂����ߊ�����M�𑗐M
334 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
335 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
336 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
337 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
338 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
339 '
340 MIN_ScwT_comOK% = 11824                 '�˂����ߑ��u����ԐM
341 MIN_ScwT_STRec% = 11857                 '�˂����ߊJ�n����M
342 MIN_ScwT_ReST% = 11858                  '�ĊJ�n����M
343 MIN_ScwT_Fin% = 11860                   '�˂����ߊ�������M
344 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
345 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
346 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
347 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
348 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
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
368 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
369 '
370 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
371 Function M% fnAssyStart
372     M_25# = 0
373     M_26# = 0
374 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
375     M_20# = MClear%                       '������
376 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
377 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
378 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
379 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
380 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
381 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
382 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
383 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
384 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
385 '    EndIf
386     ' �l�W���ߋ@�e�X�g�p ----------
387     '    'Mret% = fScewTcomChk()
388     '    '�˂����ߊJ�n
389     '    fScewTStart()
390     '    '
391     '    '���W�ړ�
392     '    '
393     '    '����xx��~
394     '    fScewTCaseStop(MScwT_Case5%)
395     '    '
396     '    '�x�[�X���j�b�gKEY
397     '    Wait M_In(MTEST_KEY%) = MOn%
398     '    '
399     '    '�ĊJ�n
400     '    fScewTReStart()
401     '    '
402     '    '���W�ړ�
403     '    '
404     '    '�˂����ߊ���
405     '    Mret% = fScewTFinish()
406     ' �l�W���߃e�X�g�I��
407     ' PIAS�e�X�g -----------
408     '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
409     '    MRet% = fnPiasWrite(MNG%)
410     '    MRet% = fnPCBNumberCheck()
411     ' PIAS�e�X�g�I�� -------
412     '�g�ݗ��ĊJ�n
413     '�v���O�������_
414         '�����ʒu��ݒ�
415     PTemp = P_Curr
416     MRtn = 0
417 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
418 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
419 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
420 '                MRtn = 1
421 '            EndIf
422 '        EndIf
423 '    EndIf
424 '    If MRtn = 1 Then
425 '        M_Out(12269) = 0            '�ʒu���ߖ�OFF
426 '        M_Out(12268) = 1            '�ʒu���ߏoON
427 '        Mov PTicketRead
428 '    Else
429 '        Mov PInitialPosition
430 '        M_Out(12269) = 0            '�ʒu���ߖ�OFF
431 '        M_Out(12268) = 1            '�ʒu���ߏoON
432 '        Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
433 '        Mvs PTicketRead             'ID�ǂ݈ʒu
434 '    EndIf
435 '
436 ' 2022/04/04 ���S�����֏����ύX �n��
437 ' PInitialPosition �ݐ� MRtn=2
438 ' PTicketRead_1 �ݐ� MRtn=1
439 '
440     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
441         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
442             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
443                 MRtn = 2
444             EndIf
445         EndIf
446     EndIf
447     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
448         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
449             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
450                 MRtn = 1
451             EndIf
452         EndIf
453     EndIf
454     fnAutoScreenComment(521)        '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
455     If MRtn = 2 Then
456         M_Out(12269) = 0            '�ʒu���ߖ�OFF
457         M_Out(12268) = 1            '�ʒu���ߏoON
458         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
459         Mvs PTicketRead             'ID�ǂ݈ʒu
460     Else
461         If MRtn = 1 Then
462             M_Out(12269) = 0            '�ʒu���ߖ�OFF
463             M_Out(12268) = 1            '�ʒu���ߏoON
464             Mvs PTicketRead             'ID�ǂ݈ʒu
465         Else
466             fErrorProcess(11,230,281,0)    '�G���[��~
467         If M_20# = MNext% Then GoTo *ASSY_ERROR_END
468             If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
469             If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
470             If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
471         EndIf
472     EndIf
473 '
474     Ovrd 100
475     '�n���h�y�ю���ɖ{�̂�������
476     *INITIAL_CHECK
477     If M_In(11264) =0 And M_In(11269) = 0 Then GoTo *CompInitial
478     fErrorProcess(11,253,281,0)
479     If M_20# = MNext% Then M_20# = MClear%
480     If M_20# = MNgProcess% Then M_20# = MAbout%
481     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
482     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
483     *CompInitial
484 '
485     '����������ʒu�ɖ߂�(�ǉ�11/19����)
486     *RE_JIG_INI
487     MRtn = 1
488     MRtn2 = 1
489     If M_In(11276) = 0 Or M_In(11277) = 0 Then  '��]�����Z���^�[�ɗ��Ă��Ȃ����
490         M_Out(12258) = 0        '���i�`���b�N�JOFF
491         M_Out(12259) = 1        '���i�`���b�N��ON
492         MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    '���i�`���b�N���o
493         M_Out(12262) = 0        '��]�X�g�b�p�[�oOFF
494         M_Out(12263) = 1        '��]�X�g�b�p�[��ON
495         MRtn2 = frInCheck(11275,1,MSETTIMEOUT05&)    '��]�X�g�b�p�[�ߒ[���o
496     EndIf
497     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompJigIni1
498     fErrorProcess(11,262,284,0) '0��262�ɕύX6/3����
499     If M_20# = MNext% Then M_20# = MClear%
500     If M_20# = MNgProcess% Then M_20# = MAbout%
501     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
502     If M_20# = MContinue% Then GoTo *RE_JIG_INI
503 *CompJigIni1
504     '
505     If M_In(11278) = 1 Then     'CW�[�ɉ�]���Ă����Ȃ�
506         M_Out(12266) = 0        '�o���u2ON��OFF
507         M_Out(12267) = 1 Dly 0.3       '�o���u2OFF��ON
508         Dly 0.3
509         M_Out(12264) = 0        '�o���u1ON��OFF
510         M_Out(12265) = 1 Dly 0.3       '�o���u1OFF��ON
511     ElseIf M_In(11279) = 1 Then 'CCW�[�ɉ�]���Ă����Ȃ�
512         M_Out(12264) = 0        '�o���u1ON��OFF
513         M_Out(12265) = 1 Dly 0.3       '�o���u1OFF��ON
514         Dly 0.3
515         M_Out(12266) = 0        '�o���u2ON��OFF
516         M_Out(12267) = 1 Dly 0.3       '�o���u2OFF��ON
517     Else
518         M_Out(12264) = 0        '�o���u1ON��OFF
519         M_Out(12265) = 1 Dly 0.3       '�o���u1OFF��ON
520         M_Out(12266) = 0        '�o���u2ON��OFF
521         M_Out(12267) = 1 Dly 0.3       '�o���u2OFF��ON
522     EndIf
523     '
524 '    Wait M_In(11276) = 1 Or M_In(11277) = 1     '��]�Z���^�[���o
525     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    '��]�Z���^�[���o
526     MRtn2 = frInCheck(11277,1,MSETTIMEOUT05&)
527 '
528     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompJigIni2
529     fErrorProcess(11,265,284,0)
530     If M_20# = MNext% Then M_20# = MClear%
531     If M_20# = MNgProcess% Then M_20# = MAbout%
532     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
533     If M_20# = MContinue% Then GoTo *RE_JIG_INI
534 *CompJigIni2
535     '
536     '
537     If M_In(11271) = 1 Then     '���i�`���b�N�Ȃ��
538         M_Out(12259) = 0        '���i�`���b�N��OFF
539         M_Out(12258) = 1        '���i�`���b�N�JON
540 '        Wait M_In(11271) = 1    '���i�`���b�N�J���o
541         M_Out(12263) = 0        '��]�X�g�b�p�[��OFF
542         M_Out(12262) = 1        '��]�X�g�b�p�[�oON
543 '        Wait M_In(11274) = 1    '��]�X�g�b�p�[�o�[���o
544     EndIf
545     '
546     M_Out(12261) = 0            '���i�N�����p�[��OFF
547     M_Out(12260) = 1            '���i�N�����p�[�oON
548 '    Wait M_In(11272) = 1        '���i�N�����p�[�o�[���o
549     '
550     M_Out(12256) = 0            '�`���b�N��OFF
551     M_Out(12257) = 1            '�`���b�N�JON
552 '    Wait M_In(11265)            '�`���b�N�J�Z���T�[ON
553 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�`���b�N�J�Z���T�[ON
554 '    If MRtn = 0 Then
555 '        fErrorProcess()         '�G���[����
556 '    EndIf
557     '
558 ''    Mov PInitialPosition
559 MRtn = 1        'MRtn������
560 '�`�P�b�gID��ǂ�
561 *RE_TICKET_READ
562 If M_20# = MContinue% Then M_20# = MClear%
563 'PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
564 'MInspGroup%(1) = 1              '����G�ԍ�
565 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
566 M_20# = MClear%                       '������
567 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
568     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
569     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
570     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
571 EndIf
572 If MRtn = 1 Then GoTo *CompRead
573 'fErrorProcess(11,214,251,0)
574 'If M_20# = MNext% Then M_20# = MClear%
575 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
576 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
577 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
578 If M_20# = MNext% Then M_20# = MPass%
579 Mvs PTicketRead_1                         '22/04/07 �ǉ� �n��
580 GoTo *ASSY_ERROR_END
581 *CompRead
582     fnAutoScreenComment(521)        '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
583     '
584     '�p���b�g���琻�i�����
585     M_Out(12269) = 0            '�ʒu���ߖ�OFF
586     M_Out(12268) = 1            '�ʒu���ߏoON
587     Mov PProductOnPltGet_2      '���i�����_
588     '
589     *RE_PLT_GET
590     '
591     M_Out(12269) = 0            '�ʒu���ߖ�OFF
592     M_Out(12268) = 1            '�ʒu���ߏoON
593     M_Out(12256) = 0            '�`���b�N��OFF
594     M_Out(12257) = 1            '�`���b�N�JON
595 '
596 '    Wait M_In(11265)            '�`���b�N�J�Z���T�[ON
597     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�`���b�N�J�Z���T�[ON
598     If MRtn = 1 Then GoTo *CompPltGet1
599     fErrorProcess(11,244,284,0)
600     If M_20# = MNext% Then M_20# = MClear%
601     If M_20# = MAbout% Or M_20# = MNgProcess% Then
602         Mov PInitialPosition    '�ޔ����[�g
603         Break
604     EndIf
605     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
606     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
607     If M_20# = MContinue% Then GoTo *RE_PLT_GET
608     *CompPltGet1
609 '
610 '    Mov PProductOnPltGet_1      '���i���(�ʒu�ύX1/14����)
611 '
612 '    Wait M_In(11262) = 1        '�ʒu���ߏo�[�Z���T�[ON
613     MRtn = frInCheck(11262,1,MSETTIMEOUT05&)   '�ʒu���ߏo�[�Z���T�[ON
614     If MRtn = 1 Then GoTo *CompPltGet2
615     fErrorProcess(11,231,282,0)
616     If M_20# = MNext% Then M_20# = MClear%
617     If M_20# = MAbout% Or M_20# = MNgProcess% Then
618         M_Out(12268) = 0            '�ʒu����
619         M_Out(12269) = 1            '�ʒu���ߖ�ON
620         Mov PProductOnPltGet_2
621         Mov PInitialPosition'�ޔ����[�g
622         Break
623     EndIf
624     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
625     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
626     If M_20# = MContinue% Then GoTo *RE_PLT_GET
627     *CompPltGet2
628 '
629     Mov PProductOnPltGet_1      '���i���
630     M_Out(12268) = 0            '�ʒu���ߏoOFF
631     M_Out(12269) = 1            '�ʒu���ߖ�ON
632 '    Wait M_In(11263) = 1        '�ʒu���ߖߒ[�Z���T�[ON
633     MRtn = frInCheck(11263,1,MSETTIMEOUT05&)   '�ʒu���ߖ߂�[�Z���T�[
634     If MRtn = 1 Then GoTo *CompPltGet3
635     fErrorProcess(11,234,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Or M_20# = MNgProcess% Then
638         Mov PProductOnPltGet_2      '�ޔ����[�g
639         Mov PInitialPosition
640         Break
641     EndIf
642     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
643     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
644     If M_20# = MContinue% Then GoTo *RE_PLT_GET
645     *CompPltGet3
646 '
647     Ovrd 30
648     Mvs PProductOnPltGet        '���i�����ʒu
649     Dly 0.1
650     M_Out(12257) = 0            '�`���b�N�JOFF
651     M_Out(12256) = 1            '�`���b�N��ON
652 '
653 '    Wait M_In(11266) = 1        '�`���b�N�Z���T�[ON
654     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�`���b�N�Z���T�[ON
655     If MRtn = 1 Then GoTo *CompPltGet4
656     M_Out(12256) = 0        '�ޔ����[�g
657     M_Out(12257) = 1
658     Dly 2.0
659     Mvs PProductOnPltGet_1
660     Mov PProductOnPltGet_2
661     M_Out(12257) = 0
662     fErrorProcess(11,245,284,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Or M_20# = MNgProcess% Then
665         Mov PInitialPosition
666         Break
667     EndIf
668     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
669     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
670     Mvs PProductOnPltGet_1
671     Mvs PProductOnPltGet
672     Dly 0.1
673     M_Out(12257) = 0            '�`���b�N�JOFF
674     M_Out(12256) = 1            '�`���b�N��ON
675     If M_20# = MContinue% Then GoTo *RE_PLT_GET
676     Mvs PProductOnPltGet
677     Dly 0.1
678     M_Out(12257) = 0            '�`���b�N�JOFF
679     M_Out(12256) = 1            '�`���b�N��ON
680     Dly 2.0
681     *CompPltGet4
682 '
683 '    Wait M_In(11264) = 1        '���i���o�Z���T�[ON
684     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '���i���o�Z���T�[�Z���T�[ON
685     If MRtn = 1 Then GoTo *CompPltGet5
686     fErrorProcess(11,252,284,0)
687     If M_20# = MNext% Then M_20# = MClear%
688     If M_20# = MAbout% Or M_20# = MNgProcess% Then
689         M_Out(12256) = 0        '�ޔ����[�g
690         M_Out(12257) = 1
691         Mvs PProductOnPltGet_1
692         Mov PProductOnPltGet_2
693         Mov PInitialPosition
694         Break
695     EndIf
696     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
697     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
698     If M_20# = MContinue% Then GoTo *RE_PLT_GET
699     *CompPltGet5
700 '
701     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
702     Mvs PProductOnPltGet_1      '���i���
703     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
704     Ovrd 60
705     Mov PProductOnPltGet_2      '���i�����_
706     '
707     '���i������ɒu��(�_�̒ǉ�,�܂�����ɂ��ϐ����ύX(9/2����))
708     Fine 1.0 , P
709     Mov PProductOnJigSet_4      '�p���b�g�����_(����3����4(9/2����))
710     Fine 0 , P
711     Mov PProductOnJigSet_3      '�p���b�g-����ԓ_(����2����3(9/2����))
712     Mov PProductOnJigSet_2      '������_(�_�̒ǉ�(9/2����))
713     '
714     *RE_JIG_SET_1
715     '
716     M_Out(12259) = 0            '����i�`���b�N��OFF
717     M_Out(12258) = 1            '����i�`���b�N�JON
718     M_Out(12261)=0              '����i�N�����p�[�����[OFF
719     M_Out(12260)=1              '����i�N�����p�[�o�[ON
720 '
721 '    Wait M_In(11270) = 1        '����i�`���b�N�J�Z���T�[ON
722     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '����i�`���b�N�J�Z���T�[ON
723     If MRtn = 1 Then GoTo *CompJIGSet1
724     fErrorProcess(11,259,284,0)
725     If M_20# = MNext% Then M_20# = MClear%
726     If M_20# = MAbout% Or M_20# = MNgProcess% Then
727         Mov PProductOnJigSet_2      '�ޔ����[�g
728         Mov PProductOnJigSet_3
729         Mov PProductOnJigSet_4
730         Mov PProductOnPltSet_2
731         Mov PProductOnPltSet_1
732         Ovrd 25
733         Mvs PProductOnPltSet
734         Dly 0.3
735         M_Out(12256)=0              '�`���b�N��OFF
736         M_Out(12257)=1              '�`���b�N�JON
737         Dly 0.5
738         Mvs PProductOnPltSet_1
739         Ovrd 100
740         Mov PProductOnPltSet_2
741         Mov PInitialPosition
742         Break
743     EndIf
744     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
745     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
746     If M_20# = MContinue% Then GoTo *RE_JIG_SET_1
747     *CompJIGSet1
748 '
749 '    Wait M_In(11272) = 1        '���i�N�����p�[�o�[�Z���T�[ON
750     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '���i�N�����p�[�o�[�Z���T�[ON
751     If MRtn = 1 Then GoTo *CompJIGSet2
752     fErrorProcess(11,257,284,0)
753     If M_20# = MNext% Then M_20# = MClear%
754     If M_20# = MAbout% Or M_20# = MNgProcess% Then
755         Mov PProductOnJigSet_2      '�ޔ����[�g
756         Mov PProductOnJigSet_3
757         Mov PProductOnJigSet_4
758         Mov PProductOnPltSet_2
759         Mov PProductOnPltSet_1
760         Ovrd 25
761         Mvs PProductOnPltSet
762         Dly 0.3
763         M_Out(12256)=0              '�`���b�N��OFF
764         M_Out(12257)=1              '�`���b�N�JON
765         Dly 2.0
766         Mvs PProductOnPltSet_1
767         Ovrd 100
768         Mov PProductOnPltSet_2
769         Mov PInitialPosition
770         Break
771     EndIf
772     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
773     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
774     If M_20# = MContinue% Then GoTo *RE_JIG_SET_1
775     *CompJIGSet2
776 '
777     Mov PProductOnJigSet_1      '������
778     Ovrd 30
779     Mvs PProductOnJigSet        '���i�u���ʒu
780     '
781 '
782 '2022.03.12 �ǉ�
783 '====================================================
784 '����i�`���b�N�J
785 *RE_JIG_SET_SP1
786     M_Out(12258) = 1            '����i�`���b�N�JON
787     M_Out(12259) = 0            '����i�`���b�N��OFF
788 '
789     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)
790     If MRtn = 1 Then GoTo *CompJIGSetSP1
791     fErrorProcess(11,258,284,0)
792     If M_20# = MNext% Then M_20# = MClear%
793     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
794 '        M_Out(12256)=0              '�`���b�N��OFF
795 '        M_Out(12257)=1              '�`���b�N�JON
796 '        Dly 2.0
797         Ovrd 5
798         Mvs PProductOnJigSet_1
799         Mov PProductOnJigSet_2
800         Mov PProductOnJigSet_3
801         Mov PProductOnJigSet_4
802         Mov PInitialPosition
803         Ovrd 30
804         Break
805     EndIf
806     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
807     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
808     If M_20# = MContinue% Then GoTo *RE_JIG_SET_SP1
809 *CompJIGSetSP1
810 '
811 '
812 '====================================================
813 '�n���h���i�`���b�N�J
814     M_Out(12256) = 0            '�n���h���i�`���b�N��OFF
815     M_Out(12257) = 1            '�n���h���i�`���b�N�JON
816 '
817     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)
818     If MRtn = 1 Then GoTo *CompJIGSetSP2
819     fErrorProcess(11,258,284,0)
820     If M_20# = MNext% Then M_20# = MClear%
821     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
822 '        M_Out(12256)=0              '�`���b�N��OFF
823 '        M_Out(12257)=1              '�`���b�N�JON
824         Dly 2.0
825         Ovrd 5
826         Mvs PProductOnJigSet_1
827         Mov PProductOnJigSet_2
828         Mov PProductOnJigSet_3
829         Mov PProductOnJigSet_4
830         Mov PInitialPosition
831         Ovrd 30
832         Break
833     EndIf
834     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
835     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
836     If M_20# = MContinue% Then GoTo *RE_JIG_SET_SP1
837 *CompJIGSetSP2
838 '
839 '
840 '====================================================
841 '����i�`���b�N��
842     M_Out(12258) = 0            '����i�`���b�N�JOFF
843     M_Out(12259) = 1            '����i�`���b�N��ON
844 '
845     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)
846     If MRtn = 1 Then GoTo *CompJIGSetSP3
847     fErrorProcess(11,258,284,0)
848     If M_20# = MNext% Then M_20# = MClear%
849     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
850         M_Out(12258)=1              '�`���b�N�JON
851         M_Out(12259)=0              '�`���b�N��OFF
852         Dly 2.0
853         Ovrd 5
854         Mvs PProductOnJigSet_1
855         Mov PProductOnJigSet_2
856         Mov PProductOnJigSet_3
857         Mov PProductOnJigSet_4
858         Mov PInitialPosition
859         Ovrd 30
860         Break
861     EndIf
862     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
863     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
864     If M_20# = MContinue% Then GoTo *RE_JIG_SET_SP1
865 *CompJIGSetSP3
866 '
867 '
868 '====================================================
869 '����i�N�����p�[�����A����
870     M_Out(12260)=0              '���i�N�����p�[�o�[OFF
871     M_Out(12261)=1              '���i�N�����p�[�����[ON
872 '    Wait M_In(11273)=1          '���i�N�����p�[�����[�Z���T�[ON
873     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '����i�N�����p�[�����[�Z���T�[ON
874     If MRtn = 1 Then GoTo *CompJIGSetSP4
875     fErrorProcess(11,256,284,0)
876     If M_20# = MNext% Then M_20# = MClear%
877     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
878         M_Out(12256)=1              '�`���b�N�oON
879         M_Out(12257)=0              '�`���b�N����OFF
880         Dly 2.0
881         Ovrd 5
882         Mvs PProductOnJigSet_1
883         Mov PProductOnJigSet_2
884         Mov PProductOnJigSet_3
885         Mov PProductOnJigSet_4
886         Mov PInitialPosition
887         Ovrd 30
888         Break
889     EndIf
890     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
891     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
892     If M_20# = MContinue% Then GoTo *RE_JIG_SET_SP1
893     *CompJIGSetSP4
894 '
895 '
896 '
897 '====================================================
898 '2022.03.12 ���ԓ����̈׍폜
899 '
900 '�ȉ��͑O�̓���
901 '    *RE_JIG_SET_2
902 '    '
903 '    M_Out(12258) = 0            '����i�`���b�N�JOFF
904 '    M_Out(12259) = 1            '����i�`���b�N��ON
905 '    '
906 ''    Wait M_In(11271) = 1
907 '    MRtn = frInCheck(11271,1,MSETTIMEOUT05&)
908 '    If MRtn = 1 Then GoTo *CompJIGSet3
909 '    fErrorProcess(11,258,284,0)
910 '    If M_20# = MNext% Then M_20# = MClear%
911 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
912 '        M_Out(12256)=0              '�`���b�N��OFF
913 '        M_Out(12257)=1              '�`���b�N�JON
914 '        Dly 2.0
915 '        Mvs PProductOnJigSet_1
916 '        Mov PProductOnJigSet_2
917 '        Mov PProductOnJigSet_3
918 '        Mov PProductOnJigSet_4
919 '        Mov PInitialPosition
920 '        Break
921 '    EndIf
922 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
923 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
924 '    If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
925 '    *CompJIGSet3
926 '    '
927 '    M_Out(12260)=0              '���i�N�����p�[�o�[OFF
928 '    M_Out(12261)=1              '���i�N�����p�[�����[ON
929 ''    Wait M_In(11273)=1          '���i�N�����p�[�����[�Z���T�[ON
930 '    MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '����i�N�����p�[�����[�Z���T�[ON
931 '    If MRtn = 1 Then GoTo *CompJIGSet4
932 '    fErrorProcess(11,256,284,0)
933 '    If M_20# = MNext% Then M_20# = MClear%
934 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
935 '        M_Out(12256)=0              '�`���b�N��OFF
936 '        M_Out(12257)=1              '�`���b�N�JON
937 '        Dly 2.0
938 '        Mvs PProductOnJigSet_1
939 '        Mov PProductOnJigSet_2
940 '        Mov PProductOnJigSet_3
941 '        Mov PProductOnJigSet_4
942 '        Mov PInitialPosition
943 '        Break
944 '    EndIf
945 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
946 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
947 '    If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
948 '    *CompJIGSet4
949 '    '
950 '    M_Out(12256)=0              '�`���b�N��OFF
951 '    M_Out(12257)=1              '�`���b�N�JON
952 ''    Wait M_In(11265)=1          '�`���b�N�J�Z���T�[ON
953 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�`���b�N�J�Z���T�[ON
954 '    If MRtn = 1 Then GoTo *CompJIGSet5
955 '    fErrorProcess(11,244,284,0)
956 '    If M_20# = MNext% Then M_20# = MClear%
957 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
958 '        Mvs PProductOnJigSet_1
959 '        Mov PProductOnJigSet_2
960 '        Mov PProductOnJigSet_3
961 '        Mov PProductOnJigSet_4
962 '        Mov PInitialPosition
963 '        Break
964 '    EndIf
965 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
966 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
967 '    If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
968 '    *CompJIGSet5
969 '2022.03.12 ���ԓ����̈׍폜
970 '====================================================
971 '
972 '
973     '
974     Mvs PProductOnJigSet_1      '������
975     Mvs PProductOnJigSet_2      '������_(�_�̒ǉ�(9�^2����))
976     '
977     *RE_JIG_SET_3
978     '
979 '    Wait M_In(11264)=0          '���i���o�Z���T�[OFF
980     MRtn = frInCheck(11264,0,MSETTIMEOUT05&)   '���i���o�Z���T�[OFF
981     If MRtn = 1 Then GoTo *CompJIGSet6
982     fErrorProcess(11,253,284,0)
983     If M_20# = MNext% Then M_20# = MClear%
984     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
985         Mvs PProductOnJigSet_1
986         Mov PProductOnJigSet_2
987         Mov PProductOnJigSet_3
988         Mov PProductOnJigSet_4
989         Mov PInitialPosition
990         Break
991     EndIf
992     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
993     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
994     If M_20# = MContinue% Then GoTo *RE_JIG_SET_3
995     *CompJIGSet6
996     '
997     Ovrd 100
998     '
999 '�˂����ߏ��ύX��������(6/9����)
1000     *RE_SCREW_SET_1
1001     '
1002     M_Out(12257) = 0            '�`���b�N�JOFF(�l�W�s�b�N�A�b�v�����΍�9/10����)
1003     M_Out(12256) = 1            '�`���b�N��ON(�l�W�s�b�N�A�b�v�����΍�9/10����)
1004 '    Wait M_In(11266) = 1        '�`���b�N�Z���T�[ON
1005     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�`���b�N�Z���T�[ON
1006     If MRtn = 1 Then GoTo *CompScrewSet1
1007     fErrorProcess(11,245,284,0)
1008     If M_20# = MNext% Then M_20# = MClear%
1009     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1010         Mvs PProductOnJigSet_1
1011         Mvs PProductOnJigSet_2
1012         Mov PProductOnJigSet_3
1013         Mov PProductOnJigSet_4
1014         Mov PInitialPosition
1015         Break
1016     EndIf
1017     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1018     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1019     If M_20# = MContinue% Then GoTo *RE_SCREW_SET_1
1020     *CompScrewSet1
1021 '�n���h�̌�����HS�˂����߂̌����ɕύX������W���K�v(6/9����)
1022     Mov PScrewSupplyHS_5        '�n���h�����ς�
1023     Mov PScrewSupplyHS_3        '����˂����{���_
1024     '�q�[�g�V���N�̃l�W����
1025     '
1026     '�q�[�g�V���N�p�l�W�����@�փl�W�����ɍs��
1027     'GoSub *ScrewSupplyHS         '�R�����g�A�E�g(9/3����)
1028     '*ScrewSupplyHS               '�R�����g�A�E�g
1029     PGetScrewPos(1) = PScrewSupplyHS_1  '�l�W�s�b�N�A�b�v���
1030     PGetScrewPos(2) = PScrewSupplyHS_2  '�l�W�����@���_
1031     PGetScrewPos(9) = PScrewSupplyHS_4  '�z���s�ǎ̂Ĉʒu
1032     PGetScrewPos(10) = PScrewSupplyHS   '�l�W�s�b�N�A�b�v�ʒu
1033 '    Mov PScrewSupplyHS_2          '�l�W�����@���_(9/30�ȉ�5�s�R�����g�A�E�g(����))
1034 '    Mvs PScrewSupplyHS_1          '�l�W�s�b�N�A�b�v���(Mov����Mvs�֕ύX(9/3����))
1035 '    Mvs PScrewSupplyHS            '�l�W�s�b�N�A�b�v
1036 '    Mvs PScrewSupplyHS_1          '�l�W�s�b�N�A�b�v���
1037 '    Mvs PScrewSupplyHS_2          '�l�W�����@���_(Mov����Mvs�֕ύX(9/3����))
1038     *RE_SCREW_GET_1
1039     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
1040     If MRtn = 1 Then GoTo *CompScrewGet1
1041     If M_20# = MNext% Then M_20# = MClear%
1042     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1043         Mov PEscapePosition_4
1044         Mov PEscapePosition_2
1045         Mov PEscapePosition
1046         Mov PInitialPosition
1047         Break
1048     EndIf
1049     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1050     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1051     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
1052     *CompScrewGet1
1053     '
1054     Mov PScrewSupplyHS_3          '����˂����{���_
1055     'Return                       '�R�����g�A�E�g(9/3����)
1056     '
1057     '�@�ԃl�W����
1058 '    Mov PScrewHeatSink1_1         '�@���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1059 '    Ovrd 5
1060 '    Mvs PScrewHeatSink1           '�@�l�W����
1061 '    Ovrd 10
1062 '    Mvs PScrewHeatSink1_1         '�@���
1063     PScrewPos(1) = PScrewHeatSink1_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1064     PScrewPos(2) = PScrewHeatSink1_0    '�˂����ߊJ�n�ʒu
1065     PScrewPos(10) = PScrewHeatSink1     '�˂����ߏI���ʒu
1066     M_Out16(12672) = 1              '�l�W���߈ʒu�ԍ����M
1067     MRtn = ScrewTight(PScrewPos,2,4.914)          '�˂����ߊJ�n
1068     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1069     If MRtn = 1 Then GoTo *CompScrew1
1070     Mov PScrewSupplyHS_3
1071     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1072     MScrewErrorCord% = MScrewErrorCord% + 1
1073     fErrorProcess(11,MScrewErrorCord%,52,0)
1074 '    fErrorProcess(11,53,52,0)
1075     If M_20# = MNext% Then M_20# = MClear%
1076     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1077         Mov PEscapePosition_3
1078         Mov PEscapePosition_2
1079         Mov PEscapePosition
1080         Mov PInitialPosition
1081         Break
1082     EndIf
1083     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1084     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1085     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
1086     *CompScrew1
1087     '
1088     '�q�[�g�V���N�p�l�W�����@�փl�W�����ɍs��
1089     'GoSub *ScrewSupplyHS         '�R�����g�A�E�g(9/10����)
1090     Mov PScrewSupplyHS_3          '����˂����{���_(�ȉ�3�s�ǉ�(9/30����))
1091     *RE_SCREW_GET_2
1092     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
1093     If MRtn = 1 Then GoTo *CompScrewGet2
1094     If M_20# = MNext% Then M_20# = MClear%
1095     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1096         Mov PEscapePosition_4
1097         Mov PEscapePosition_2
1098         Mov PEscapePosition
1099         Mov PInitialPosition
1100         Break
1101     EndIf
1102     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1103     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1104     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
1105     *CompScrewGet2
1106     '
1107     Mov PScrewSupplyHS_3          '����˂����{���_
1108     '
1109     '�A�ԃl�W����
1110 '    Mov PScrewHeatSink2_1         '�A���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1111 '    Ovrd 5
1112 '    Mvs PScrewHeatSink2           '�A�l�W����
1113 '    Ovrd 10
1114 '    Mvs PScrewHeatSink2_1         '�A���
1115     PScrewPos(1) = PScrewHeatSink2_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1116     PScrewPos(2) = PScrewHeatSink2_0    '�˂����ߊJ�n�ʒu
1117     PScrewPos(10) = PScrewHeatSink2     '�˂����ߏI���ʒu
1118     M_Out16(12672) = 2              '�l�W���߈ʒu�ԍ����M
1119     MRtn = ScrewTight(PScrewPos,6,4.914)          '�˂����ߊJ�n
1120     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1121     If MRtn = 1 Then GoTo *CompScrew2
1122     Mov PScrewSupplyHS_3
1123     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1124     MScrewErrorCord% = MScrewErrorCord% + 2
1125     fErrorProcess(11,MScrewErrorCord%,52,0)
1126 '    fErrorProcess(11,54,52,0)
1127     If M_20# = MNext% Then M_20# = MClear%
1128     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1129         Mov PEscapePosition_3
1130         Mov PEscapePosition_2
1131         Mov PEscapePosition
1132         Mov PInitialPosition
1133         Break
1134     EndIf
1135     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1136     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1137     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
1138     *CompScrew2
1139     '
1140     '
1141     '
1142     '
1143     Mov PScrewSupplyPlate_3          '�l�W�s�b�N�A�b�v�������킹�ʒu
1144     '
1145 '    *RE_SCREW_SET_2
1146     '
1147     '���������ɂ�90�x��](���񏈗���4/13����)
1148 '    M_Out(12262) = 0             '��]�X�g�b�p�[�oOFF
1149 '    M_Out(12263) = 1             '��]�X�g�b�p�[��ON
1150 '    '
1151 ''    Wait M_In(11275) = 1         '��]�X�g�b�p�[�ߒ[���o�Z���T�[ON
1152 '    MRtn = frInCheck(11275,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[�Z���T�[ON
1153 '    If MRtn = 1 Then GoTo *CompScrewSet2
1154 '    fErrorProcess(11,262,284,0)
1155 '    If M_20# = MNext% Then M_20# = MClear%
1156 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1157 '        Mov PEscapePosition_3
1158 '        Mov PEscapePosition_2
1159 '        Mov PEscapePosition
1160 '        Mov PInitialPosition
1161 '        Break
1162 '    EndIf
1163 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1164 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1165 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_2
1166 '    *CompScrewSet2
1167     M_25# = 1                   '���񏈗��FCW��]�J�n(�f�o�b�O��)
1168 '
1169     '�����ʂ̃l�W����
1170     '
1171     '���p�l�W�����@�փl�W�����ɍs��
1172     'GoSub *ScrewSupplyPlate
1173     '*ScrewSupplyPlate
1174     PGetScrewPos(1) = PScrewSupplyPlate_1  '�l�W�s�b�N�A�b�v���
1175     PGetScrewPos(2) = PScrewSupplyPlate_2  '�l�W�����@���_
1176     PGetScrewPos(9) = PScrewSupplyPlate_7  '�z���s�ǖ߂��ʒu
1177     PGetScrewPos(10) = PScrewSupplyPlate   '�l�W�s�b�N�A�b�v�ʒu
1178 *RE_SCREW_GET_3
1179     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        '�l�W�󂯎��J�n
1180 '
1181     If MRtn = 1 Then GoTo *CompScrewGet3
1182     If M_20# = MNext% Then M_20# = MClear%
1183     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1184         Mov PEscapePosition_3
1185         Mov PEscapePosition_2
1186         Mov PEscapePosition
1187         Mov PInitialPosition
1188         Break
1189     EndIf
1190     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1191     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1192     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
1193     *CompScrewGet3
1194     '
1195     Mov PScrewSupplyPlate_3          '�l�W�����@-����Ԓ��ԓ_(�ǉ�9/10����)
1196     '�^�N�g�Z�k�̂��ߒǉ��A�C��2/3����
1197 '    *RE_CW_ROT
1198 '    If M_20# = MContinue% Then
1199 '        M_Out(12264) = 1 Dly 0.3     'CW1�o���uON
1200 '    EndIf
1201 '    MRtn = frInCheck(11278,1,MSETTIMEOUT05&)   'CW�[�Z���T�[ON
1202 '    If MRtn = 1 Then GoTo *CompScrewSet3
1203 '    fErrorProcess(11,264,284,0)
1204 '    If M_20# = MNext% Then M_20# = MClear%
1205 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1206     M_20# = MClear%
1207 *LOOP1
1208     If M_20# = MContinue% Then
1209         M_25# = 1
1210         M_20# = MClear%
1211     EndIf
1212     If M_26# <> 0 Then GoTo *LOOP1END Else GoTo *LOOP1
1213 *LOOP1END
1214 '
1215     If M_26# = 1 Then GoTo *Set1End                            '����I���Ȃ�G���[�����Ȃ�
1216     If M_In(11275) = 1 And M_In(11278) = 1 Then GoTo *Set1End  '�����݂�����I���Ɠ����Z���T�[��ԂȂ�G���[�����Ȃ�
1217     If M_In(11275) = 0 Then                                    '�Z���T�[�̏�Ԃ����ăG���[���o��
1218         fErrorProcess(11,262,284,0)
1219     ElseIf M_In(11278) = 0 Then
1220         fErrorProcess(11,264,284,0)
1221     EndIf
1222     M_26# = 0
1223     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1224         Mov PEscapePosition_3
1225         Mov PScrewSupplyPlate_7  '�z���s�ǖ߂��ʒu
1226         M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1227         Dly 0.2
1228         '�j��ON
1229         M_Out(Y6B_VB1)=1 '�^��j��ON
1230         '�r�b�g��]
1231         M_Out(Y61_Driver)=1
1232         Dly 0.5
1233         '                '
1234         Ovrd 100
1235         JOvrd M_NJovrd
1236         Spd M_NSpd
1237         '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1238         Mov PScrewSupplyPlate_7,10
1239         Mov PScrewSupplyPlate_7
1240         Dly 0.1
1241         Mov PScrewSupplyPlate_7,10
1242         Mov PScrewSupplyPlate_7
1243 '
1244         '�l�W�����҂�
1245         Wait M_In(11268) = 0
1246         '�r�b�g��]��~
1247         M_Out(Y61_Driver)=0
1248         Dly 0.1
1249         '�j��OFF
1250         M_Out(Y6B_VB1)=0 '�^��j��OFF
1251         Mov PEscapePosition_2
1252         Mov PEscapePosition
1253         Mov PInitialPosition
1254         Break
1255     EndIf
1256     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1257     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1258 '    If M_20# = MContinue% Then GoTo *RE_CW_ROT
1259 '    *CompScrewSet3
1260     If M_20# = MNext% Then M_20# = MClear%
1261     If M_20# = MContinue% Then GoTo *LOOP1
1262 '
1263 *Set1End
1264 '    M_26# = 0                       '�������^�C�~���O�ύX6/9����
1265 '
1266     Mov PScrewSupplyPlate_4          '������_
1267     'Return                          '�R�����g�A�E�g(9/3����)
1268     '
1269     '����L�l�W����(�d�l�ύX�ɂ�PScrewPlateL1��PScrewPlateL�֕ύX)
1270 '    Mov PScrewPlateL1_1            '�@���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1271 '    Ovrd 5
1272 '    Mvs PScrewPlateL1              '�@�l�W����
1273 '    Ovrd 10
1274 '    Mvs PScrewPlateL1_1            '�@���
1275     PScrewPos(1) = PScrewPlateL1_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1276     PScrewPos(2) = PScrewPlateL1_0    '�˂����ߊJ�n�ʒu
1277     PScrewPos(10) = PScrewPlateL1     '�˂����ߏI���ʒu
1278     M_Out16(12672) = 3              '�l�W���߈ʒu�ԍ����M
1279     MRtn = ScrewTight(PScrewPos,4,4.914)          '�˂����ߊJ�n
1280     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1281     If MRtn = 1 Then GoTo *CompScrew3
1282     Mov PScrewSupplyPlate_4
1283     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1284     MScrewErrorCord% = MScrewErrorCord% + 3
1285     fErrorProcess(11,MScrewErrorCord%,52,0)
1286 '    fErrorProcess(11,55,52,0)
1287     If M_20# = MNext% Then M_20# = MClear%
1288     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1289         Mov PEscapePosition_3
1290         Mov PEscapePosition_2
1291         Mov PEscapePosition
1292         Mov PInitialPosition
1293         Break
1294     EndIf
1295     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1296     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1297     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
1298     *CompScrew3
1299     M_26# = 0                       '�������^�C�~���O�ύX6/9����
1300 '
1301 '    '�ȉ�17�s�d�l�ύX�ɂ��R�����g�A�E�g(11/4����)
1302 '    '���p�l�W�����@�փl�W�����ɍs��
1303 '    'GoSub *ScrewSupplyPlate       '�R�����g�A�E�g(9/9����)
1304     Mov PScrewSupplyPlate_4          '������_(�ȉ�5�s�ǉ�(9/30����))
1305     Mov PScrewSupplyPlate_3          '����-�l�W�����@�Ԓ��ԓ_
1306     *RE_SCREW_GET_4
1307     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        '�l�W�󂯎��J�n
1308     If MRtn = 1 Then GoTo *CompScrewGet4
1309     If M_20# = MNext% Then M_20# = MClear%
1310     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1311         Mov PEscapePosition_3
1312         Mov PEscapePosition_2
1313         Mov PEscapePosition
1314         Mov PInitialPosition
1315         Break
1316     EndIf
1317     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1318     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1319     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
1320     *CompScrewGet4
1321     '
1322     Mov PScrewSupplyPlate_3          '�l�W�����@-����Ԓ��ԓ_
1323     Mov PScrewSupplyPlate_4          '������_
1324 '    '
1325 '    '�A�ԃl�W����
1326 ''    Mov PScrewPlateL2_1            '�A���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1327 ''    Ovrd 5
1328 ''    Mvs PScrewPlateL2              '�A�l�W����
1329 ''    Ovrd 10
1330 ''    Mvs PScrewPlateL2_1            '�A���
1331     PScrewPos(1) = PScrewPlateL2_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1332     PScrewPos(2) = PScrewPlateL2_0    '�˂����ߊJ�n�ʒu
1333     PScrewPos(10) = PScrewPlateL2     '�˂����ߏI���ʒu
1334     M_Out16(12672) = 4              '�l�W���߈ʒu�ԍ����M
1335     MRtn = ScrewTight(PScrewPos,4,4.914)          '�˂����ߊJ�n
1336     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1337     If MRtn = 1 Then GoTo *CompScrew4
1338     Mov PScrewSupplyPlate_4
1339     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1340     MScrewErrorCord% = MScrewErrorCord% + 4
1341     fErrorProcess(11,MScrewErrorCord%,52,0)
1342 '    fErrorProcess(11,56,52,0)
1343     If M_20# = MNext% Then M_20# = MClear%
1344     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1345         Mov PEscapePosition_3
1346         Mov PEscapePosition_2
1347         Mov PEscapePosition
1348         Mov PInitialPosition
1349         Break
1350     EndIf
1351     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1352     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1353     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
1354     *CompScrew4
1355 '
1356     Mov PScrewSupplyPlate_4        '�����]�ɂ����炩���߉��������(�ǉ�9/10����)
1357     '
1358     '
1359 '    *RE_SCREW_SET_3
1360 '    '
1361 '    M_Out(12265) = 1 Dly 0.3       'CW2�o���uON
1362 ''    Wait M_In(11277) = 1           'CCW�Z���^�[���o�Z���T�[ON
1363 '    MRtn = frInCheck(11277,1,MSETTIMEOUT05&)
1364 '    If MRtn = 1 Then GoTo *CompScrewSet4
1365 '    fErrorProcess(11,265,284,0)
1366 '    If M_20# = MNext% Then M_20# = MClear%
1367 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1368 '        Mov PEscapePosition_3
1369 '        Mov PEscapePosition_2
1370 '        Mov PEscapePosition
1371 '        Mov PInitialPosition
1372 '        Break
1373 '    EndIf
1374 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1375 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1376 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_3
1377 '    *CompScrewSet4
1378 '    '
1379 '    *RE_SCREW_SET_4
1380 '    '
1381 '    M_Out(12266) = 1 Dly 0.3       'CCW1�o���uON
1382 '    Wait M_In(11279) = 1           'CCW�[���o�Z���T�[ON
1383 '    MRtn = frInCheck(11279,1,MSETTIMEOUT05&)   'CCW�[���o�Z���T�[ON(�^�N�g�Z�k�̂��߃R�����g�A�E�g2/3����)
1384 '    If MRtn = 1 Then GoTo *CompScrewSet5
1385 '    fErrorProcess(11,266,284,0)
1386 '    If M_20# = MNext% Then M_20# = MClear%
1387 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1388 '        Mov PEscapePosition_3
1389 '        Mov PEscapePosition_2
1390 '        Mov PEscapePosition
1391 '        Mov PInitialPosition
1392 '        Break
1393 '    EndIf
1394 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1395 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1396 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_4
1397 '    *CompScrewSet5
1398     '
1399     '�E���ʂ̃l�W����
1400     '
1401     '���p�l�W�����@�փl�W�����ɍs��
1402     'GoSub *ScrewSupplyPlate       '�R�����g�A�E�g(9/9����)
1403     Mov PScrewSupplyPlate_3          '����-�l�W�����@�Ԓ��ԓ_(�ȉ�4�s�ǉ�(9/30����))
1404     '����180�x��]
1405     M_25# = 2
1406     *RE_SCREW_GET_5
1407     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        '�l�W�󂯎��J�n
1408     If MRtn = 1 Then GoTo *CompScrewGet5
1409     If M_20# = MNext% Then M_20# = MClear%
1410     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1411         Mov PEscapePosition_3
1412         Mov PEscapePosition_2
1413         Mov PEscapePosition
1414         Mov PInitialPosition
1415         Break
1416     EndIf
1417     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1418     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1419     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
1420     *CompScrewGet5
1421     Mov PScrewSupplyPlate_3          '�l�W�����@-����Ԓ��ԓ_
1422 '
1423 '    *RE_CCW_ROT
1424 '    If M_20# = MContinue% Then
1425 '        M_Out(12266) = 1 Dly 0.3     'CW1�o���uON
1426 '    EndIf
1427 '    'CCW�Z���T�[���o(�^�N�g�Z�k�̂��߈ړ��A�C���j
1428 '    MRtn = frInCheck(11279,1,MSETTIMEOUT05&)   'CCW�[���o�Z���T�[ON
1429 '    If MRtn = 1 Then GoTo *CompScrewSet5
1430 '    fErrorProcess(11,266,284,0)
1431 '    If M_20# = MNext% Then M_20# = MClear%
1432     M_20# = MClear%
1433 *LOOP2
1434     If M_20# = MContinue% Then
1435         M_25# = 2
1436         M_20# = MClear%
1437     EndIf
1438     If M_26# <> 0 Then GoTo *LOOP2END Else GoTo *LOOP2
1439 *LOOP2END
1440 '
1441     If M_26# = 1 Then GoTo *Set2End                            '����I���Ȃ�G���[�����Ȃ�
1442     If M_In(11279) = 1 Then GoTo *Set2End  '�����݂�����I���Ɠ����Z���T�[��ԂȂ�G���[�����Ȃ�
1443     If M_In(11277) = 0 Then                                    '�Z���T�[�̏�Ԃ����ăG���[���o��
1444         fErrorProcess(11,265,284,0)
1445     ElseIf M_In(11279) = 0 Then
1446         fErrorProcess(11,266,284,0)
1447     EndIf
1448     M_26# = 0
1449     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1450         Mov PEscapePosition_3
1451         Mov PScrewSupplyPlate_7  '�z���s�ǖ߂��ʒu
1452         M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1453         Dly 0.2
1454         '�j��ON
1455         M_Out(Y6B_VB1)=1 '�^��j��ON
1456         '�r�b�g��]
1457         M_Out(Y61_Driver)=1
1458         Dly 0.5
1459         '                '
1460         Ovrd 100
1461         JOvrd M_NJovrd
1462         Spd M_NSpd
1463         '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1464         Mov PScrewSupplyPlate_7,10
1465         Mov PScrewSupplyPlate_7
1466         Dly 0.1
1467         Mov PScrewSupplyPlate_7,10
1468         Mov PScrewSupplyPlate_7
1469 '
1470         '�l�W�����҂�
1471         Wait M_In(11268) = 0
1472         '�r�b�g��]��~
1473         M_Out(Y61_Driver)=0
1474         Dly 0.1
1475         '�j��OFF
1476         M_Out(Y6B_VB1)=0 '�^��j��OFF
1477         Mov PEscapePosition_2
1478         Mov PEscapePosition
1479         Mov PInitialPosition
1480         Break
1481     EndIf
1482     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1483     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1484 '    If M_20# = MContinue% Then GoTo *RE_CCW_ROT
1485     If M_20# = MNext% Then M_20# = MClear%
1486     If M_20# = MContinue% Then GoTo *LOOP2
1487 '    *CompScrewSet5
1488 *Set2End
1489 '    M_26# = 0                       '�������^�C�~���O�ύX6/9����
1490 '
1491     Mov PScrewSupplyPlate_4          '������_
1492     '
1493     '�@�ԃl�W����(�d�l�ύX�ɂ�PScrewPlateR1��PScrewPlateR�֕ύX)
1494 '    Mov PScrewPlateR1_1            '�@���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1495 '    Ovrd 5
1496 '    Mvs PScrewPlateR1              '�@�l�W����
1497 '    Ovrd 10
1498 '    Mvs PScrewPlateR1_1            '�@���
1499     PScrewPos(1) = PScrewPlateR1_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1500     PScrewPos(2) = PScrewPlateR1_0    '�˂����ߊJ�n�ʒu
1501     PScrewPos(10) = PScrewPlateR1     '�˂����ߏI���ʒu
1502     M_Out16(12672) = 5              '�l�W���߈ʒu�ԍ����M
1503     MRtn = ScrewTight(PScrewPos,4,4.914)          '�˂����ߊJ�n
1504     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1505     If MRtn = 1 Then GoTo *CompScrew5
1506     Mov PScrewSupplyPlate_4
1507     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1508     MScrewErrorCord% = MScrewErrorCord% + 5
1509     fErrorProcess(11,MScrewErrorCord%,52,0)
1510 '    fErrorProcess(11,57,52,0)
1511     If M_20# = MNext% Then M_20# = MClear%
1512     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1513         Mov PEscapePosition_3
1514         Mov PEscapePosition_2
1515         Mov PEscapePosition
1516         Mov PInitialPosition
1517         Break
1518     EndIf
1519     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1520     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1521     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
1522     *CompScrew5
1523     M_26# = 0                       '�������^�C�~���O�ύX6/9����
1524 '
1525 '
1526     '�ȉ�18�s�d�l�ύX�ɂ��R�����g�A�E�g(11/4����)
1527 '    '���p�l�W�����@�փl�W�����ɍs��
1528 '    'GoSub *ScrewSupplyPlate       '�R�����g�A�E�g(9/9����)
1529     Mov PScrewSupplyPlate_4          '������_(�ȉ�5�s�ǉ�(9/30����))
1530     Mov PScrewSupplyPlate_3          '����-�l�W�����@�Ԓ��ԓ_
1531     '
1532     *RE_SCREW_GET_6
1533     '
1534     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        '�l�W�󂯎��J�n
1535     If MRtn = 1 Then GoTo *CompScrewGet6
1536     If M_20# = MNext% Then M_20# = MClear%
1537     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1538         Mov PEscapePosition_3
1539         Mov PEscapePosition_2
1540         Mov PEscapePosition
1541         Mov PInitialPosition
1542         Break
1543     EndIf
1544     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1545     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1546     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
1547     *CompScrewGet6
1548     Mov PScrewSupplyPlate_3          '�l�W�����@-����Ԓ��ԓ_
1549     Mov PScrewSupplyPlate_4          '������_
1550 '    '
1551 '    '�A�ԃl�W����
1552 ''    Mov PScrewPlateR2_1            '�A���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1553 ''    Ovrd 5
1554 ''    Mvs PScrewPlateR2              '�A�l�W����
1555 ''    Ovrd 10
1556 ''    Mvs PScrewPlateR2_1            '�A���
1557     PScrewPos(1) = PScrewPlateR2_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1558     PScrewPos(2) = PScrewPlateR2_0    '�˂����ߊJ�n�ʒu
1559     PScrewPos(10) = PScrewPlateR2     '�˂����ߏI���ʒu
1560     M_Out16(12672) = 6              '�l�W���߈ʒu�ԍ����M
1561     MRtn = ScrewTight(PScrewPos,4,4.914)          '�˂����ߊJ�n
1562     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1563     If MRtn = 1 Then GoTo *CompScrew6
1564     Mov PScrewSupplyPlate_4
1565     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1566     MScrewErrorCord% = MScrewErrorCord% + 6
1567     fErrorProcess(11,MScrewErrorCord%,52,0)
1568 '    fErrorProcess(11,58,52,0)
1569     If M_20# = MNext% Then M_20# = MClear%
1570     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1571         Mov PEscapePosition_3
1572         Mov PEscapePosition_2
1573         Mov PEscapePosition
1574         Mov PInitialPosition
1575         Break
1576     EndIf
1577     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1578     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1579     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
1580     *CompScrew6
1581     '
1582     Mov PScrewSupplyHS_3           '����˂����{���_
1583     '
1584     '����90�x��]
1585 '    *RE_CENTER_POS
1586 '    M_Out(12267) = 1 Dly 0.3       'CCW2�o���uON
1587 ''    Wait M_In(11276) = 1           'CW�Z���^�[���o
1588 '    MRtn = frInCheck(11276,1,MSETTIMEOUT05&)   'CW�Z���^�[���o
1589 '    If MRtn = 1 Then GoTo *CompSenterPos1
1590 '    fErrorProcess(11,265,284,0)
1591 '    If M_20# = MNext% Then M_20# = MClear%
1592 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1593 '        Mov PEscapePosition_2
1594 '        Mov PEscapePosition
1595 '        Mov PInitialPosition
1596 '        Break
1597 '    EndIf
1598 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1599 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1600 '    If M_20# = MContinue% Then GoTo *RE_CENTER_POS
1601 '    *CompSenterPos1
1602 '    '
1603 '    M_Out(12263) = 0               '��]�X�g�b�p�[��OFF
1604 '    M_Out(12262) = 1               '��]�X�g�b�p�[�oON
1605 '    '
1606 ''    Wait M_In(11274) = 1           '��]�X�g�b�p�[�o�[���oON
1607 '    MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '��]�X�g�b�p�[�o�[���oON
1608 '    If MRtn = 1 Then GoTo *CompSenterPos2
1609 '    fErrorProcess(11,263,284,0)
1610 '    If M_20# = MNext% Then M_20# = MClear%
1611 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1612 '        Mov PEscapePosition_2
1613 '        Mov PEscapePosition
1614 '        Mov PInitialPosition
1615 '        Break
1616 '    EndIf
1617 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1618 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1619 '    If M_20# = MContinue% Then GoTo *RE_CENTER_POS
1620 '    *CompSenterPos2
1621 '    '
1622 '    '
1623 '    '����琻�i�����o��
1624 '    '
1625 ''    Mov PProductOnJigGet_6          '�l�W�s�b�N�A�b�v�������킹�ʒu
1626     M_25# = 3       '�ʒu�b��
1627     Mov PProductOnJigGet_5          '���i�`���b�N-�l�W�s�b�N�A�b�v�������킹���Ԉʒu
1628 '
1629     Mov PProductOnJigGet_4          '���i�s�b�N�A�b�v�������킹�ʒu
1630     M_20# = MClear%
1631 *LOOP3
1632     If M_20# = MContinue% Then
1633         M_25# = 3
1634         M_20# = MClear%
1635     EndIf
1636     If M_26# <> 0 Then GoTo *LOOP3END Else GoTo *LOOP3
1637 *LOOP3END
1638     If M_26# = 1 Then GoTo *Set3End                            '����I���Ȃ�G���[�����Ȃ�
1639     If M_In(11274) = 1 And M_In(11276) = 1 Then GoTo *Set3End  '�����݂�����I���Ɠ����Z���T�[��ԂȂ�G���[�����Ȃ�
1640     If M_In(11276) = 0 Then                                    '�Z���T�[�̏�Ԃ����ăG���[���o��
1641         fErrorProcess(11,265,284,0)
1642     ElseIf M_In(11274) = 0 Then
1643         fErrorProcess(11,263,284,0)
1644     EndIf
1645     M_26# = 0
1646     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1647         Mov PProductOnJigGet_4
1648         Mov PProductOnJigGet_3
1649         Mov PProductOnPltSet_3
1650         Mov PProductOnPltSet_2
1651         Mov PInitialPosition
1652         Break
1653     EndIf
1654     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1655     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1656 '    If M_20# = MContinue% Then GoTo *RE_CCW_ROT
1657     If M_20# = MNext% Then M_20# = MClear%
1658     If M_20# = MContinue% Then GoTo *LOOP3
1659 '    *CompScrewSet5
1660 *Set3End
1661     M_26# = 0
1662 '�˂����ߏ��ύX�����܂�(6/9����)
1663     *RE_JIG_GET_1
1664     '
1665     M_Out(12259) = 0                '����i�`���b�N��OFF
1666     M_Out(12258) = 1                '����i�`���b�N�JON
1667     M_Out(12261) = 0                '���i�N�����p�[�����[OFF
1668     M_Out(12260) = 1                '���i�N�����p�[�o�[ON
1669     M_Out(12256)= 0                 '���i�`���b�N��OFF
1670     M_Out(12257)= 1                 '���i�`���b�N�JON
1671     '
1672 '    Wait M_In(11265)=1              '���i�`���b�N�J�Z���T�[ON
1673     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '���i�`���b�N�J�Z���T�[ON
1674     If MRtn = 1 Then GoTo *CompJigGet1
1675     fErrorProcess(11,244,284,0)
1676     If M_20# = MNext% Then M_20# = MClear%
1677     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1678         Mov PProductOnJigGet_3
1679         Mov PProductOnPltSet_3
1680         Mov PProductOnPltSet_2
1681         Mov PInitialPosition
1682         Break
1683     EndIf
1684     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1685     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1686     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1687     *CompJigGet1
1688     '
1689 '    Wait M_In(11272)=1              '����i�N�����p�[�Z���T�[�J���[ON
1690     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '����i�N�����p�[�Z���T�[����[ON
1691     If MRtn = 1 Then GoTo *CompJigGet2
1692     fErrorProcess(11,257,284,0)
1693     If M_20# = MNext% Then M_20# = MClear%
1694     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1695         Mov PProductOnJigGet_3
1696         Mov PProductOnPltSet_3
1697         Mov PProductOnPltSet_2
1698         Mov PInitialPosition
1699         Break
1700     EndIf
1701     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1702     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1703     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1704     *CompJigGet2
1705     '
1706     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)
1707     If MRtn = 1 Then GoTo *CompJigGet3
1708     fErrorProcess(11,259,284,0)
1709     If M_20# = MNext% Then M_20# = MClear%
1710     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1711         Mov PProductOnJigGet_3
1712         Mov PProductOnPltSet_3
1713         Mov PProductOnPltSet_2
1714         Mov PInitialPosition
1715         Break
1716     EndIf
1717     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1718     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1719     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1720     *CompJigGet3
1721     '
1722     Ovrd 100
1723     'Mov PProductOnJigGet_3          '�n���h����](�R�����g�A�E�g9/10����)
1724     Mov PProductOnJigGet_2          '��������_
1725     Mvs PProductOnJigGet_1          '������
1726     Ovrd 30
1727     Mvs PProductOnJigGet            '���i���o���ʒu
1728     *RETRY_PRODUCT_ON_JIG_GET_2
1729     M_Out(12257)=0                  '���i�`���b�N�JOFF
1730     M_Out(12256)=1                  '���i�`���b�N��ON
1731 '    Wait M_In(11266)=1              '���i�`���b�N�Z���T�[ON
1732     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '���i�`���b�N�Z���T�[ON
1733     If MRtn = 1 Then GoTo *CompJigGet4
1734 '
1735     fErrorProcess(11,245,284,0)
1736     If M_20# = MNext% Then M_20# = MClear%
1737     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1738         M_Out(12256)=0                  '���i�`���b�N��OFF
1739         M_Out(12257)=1                  '���i�`���b�N�JON
1740         Dly 2.0
1741         Mvs PProductOnJigGet_1
1742         Mov PProductOnJigGet_2
1743         Mov PProductOnJigGet_3
1744         Mov PProductOnPltSet_3
1745         Mov PProductOnPltSet_2
1746         Mov PInitialPosition
1747         Break
1748     EndIf
1749     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1750     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1751     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1752     *CompJigGet4
1753     '
1754 '    Wait M_In(11264)=1              '���i���o�Z���T�[ON
1755     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '���i���o�Z���T�[ON
1756     If MRtn = 1 Then GoTo *CompJigGet5
1757     fErrorProcess(11,252,284,0)
1758     If M_20# = MNext% Then M_20# = MClear%
1759     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1760         M_Out(12256)=0                  '���i�`���b�N��OFF
1761         M_Out(12257)=1                  '���i�`���b�N�JON
1762         Dly 2.0
1763         Mvs PProductOnJigGet_1
1764         Mov PProductOnJigGet_2
1765         Mov PProductOnJigGet_3
1766         Mov PProductOnPltSet_3
1767         Mov PProductOnPltSet_2
1768         Mov PInitialPosition
1769         Break
1770     EndIf
1771     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1772     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1773     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1774     *CompJigGet5
1775     '
1776     Dly 0.1
1777     Accel 50 , 100
1778     Mvs PProductOnJigGet_1          '������
1779     Accel 100 , 100
1780     Ovrd 100
1781     Mov PProductOnJigGet_2          '��������_
1782     Mov PProductOnJigGet_3          '�p���b�g-����ԓ_
1783     '
1784     '���i���p���b�g�ɒu��
1785     '
1786     Ovrd 60
1787     Mov PProductOnPltSet_3          '�ʉߓ_
1788     Mov PProductOnPltSet_2          '�p���b�g���_
1789     Mov PProductOnPltSet_1          '�p���b�g���
1790     Ovrd 10
1791     Mvs PProductOnPltSet            '�p���b�g�u���ʒu
1792     Dly 0.2
1793     '
1794     *RE_PLT_SET_1
1795     '
1796     M_Out(12256)=0                  '���i�`���b�N��OFF
1797     M_Out(12257)=1                  '���i�`���b�N�JON
1798     '
1799 '    Wait M_In(11265)=1              '���i�`���b�N�J�Z���T�[ON
1800     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '���i�`���b�N�J�Z���T�[ON
1801     If MRtn = 1 Then GoTo *CompPltSet1
1802     fErrorProcess(11,244,284,0)
1803     If M_20# = MNext% Then M_20# = MClear%
1804     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1805         Mvs PProductOnPltSet_1          '�p���b�g���
1806         Mov PProductOnPltSet_2          '�p���b�g���_
1807         Mov PInitialPosition
1808         Break
1809     EndIf
1810     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1811     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1812     If M_20# = MContinue% Then GoTo *RE_PLT_SET_1
1813     *CompPltSet1
1814     '
1815     Ovrd 100
1816     Mvs PProductOnPltSet_1          '�p���b�g���
1817     Mov PProductOnPltSet_2          '�p���b�g���_
1818     '
1819     *RE_PLT_SET_2
1820     '
1821 '    Wait M_In(11264) = 0            '���i���o�Z���T�[OFF
1822     MRtn = frInCheck(11264,0,MSETTIMEOUT05&)   '���i���o�Z���T�[OFF
1823     If MRtn = 1 Then GoTo *CompPltSet2
1824     fErrorProcess(11,253,284,0)
1825     If M_20# = MNext% Then M_20# = MClear%
1826     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1827         Mov PInitialPosition
1828         Break
1829     EndIf
1830     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1831     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1832     If M_20# = MContinue% Then GoTo *RE_PLT_SET_2
1833     *CompPltSet2
1834     '
1835 '    Mov PInitialPosition            '�v���O�������_
1836     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
1837     Mov PTicketRead_1
1838     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1839     M_20# = MAssyOK%          '����I��
1840 '
1841 *ASSY_ERROR_END
1842     M_Out(12268) = 0            '�ʒu���ߏoOFF
1843     M_Out(12269) = 1            '�ʒu���ߖ�ON
1844 *AssyEnd
1845 *fnAssyStart_FEndPosi
1846     Exit Function
1847 FEnd
1848 '
1849 '��fnPiasCheck
1850 ''' <summary>
1851 ''' PIAS�`�P�b�g�Ǎ���
1852 ''' </summary>
1853 ''' <returns>   0 : NG
1854 '''             1 : OK(�Ǎ��݊���)
1855 ''' </returns>
1856 ''' <remarks>
1857 ''' Date   : 2021/07/07 : M.Hayakawa
1858 ''' </remarks>'
1859 Function M% fnPiasCheck
1860     fnPiasCheck = 0
1861     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1862     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1863 '
1864 *RETRY_PIAS
1865     M_20# = MClear%
1866     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1867     '
1868     '�yID�`�P�b�g�ǂݍ��݁z
1869     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1870     MInspGroup%(1) = 1              '����G�ԍ�
1871     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1872 '
1873     '�G���[�̏ꍇ
1874     If MRtn <> 1 Then
1875         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1876         If MRtn <> 1 Then
1877             'D720 -> D1300 �R�s�[�v��
1878             M_Out(12565) = 1
1879             Dly 0.5
1880             M_Out(12565) = 0
1881             '�G���[�����L�q
1882             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1883             'GOT KEY���͑҂�
1884             MKeyNumber = fnKEY_WAIT()
1885             '
1886             Select MKeyNumber
1887                 Case MNext%         '���ւ�I�������ꍇ
1888                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1889                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1890                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1891                     Break
1892                 Case MAbout%        '��~��I�������ꍇ
1893                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1894                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1895                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1896                     Break
1897                 Case MNgProcess%    'NG��I�������ꍇ
1898                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1899                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1900                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1901                     Break
1902                 Case MContinue%     '�p����I�������ꍇ
1903                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1904                     M_20# = MContinue%
1905                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1906                     Break
1907             End Select
1908         EndIf
1909     EndIf
1910 '----------D720 -> D1300 �R�s�[�v��----------
1911     M_Out(12565) = 1
1912     Dly 0.5
1913     M_Out(12565) = 0
1914 '----------�ʐM�m�F������----------
1915     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1916     MRtn = 0                ' ������
1917     M_20# = MClear%         ' ������
1918     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1919     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1920     If MRtn <> 1 Then
1921         If M_20# = MContinue% Then
1922             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1923         Else
1924             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1925         EndIf
1926     EndIf
1927 '----------�H�������m�F----------
1928     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1929     MRtn = 0                ' ������
1930     M_20# = MClear%         ' ������
1931     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1932     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1933     If MRtn <> 1 Then
1934         If M_20# = MContinue% Then
1935             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1936         Else
1937             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1938         EndIf
1939     EndIf
1940     '
1941     fnPiasCheck = 1
1942     *fnPiasCheck_End
1943     Exit Function
1944 FEnd
1945 '
1946 '��fnPCComuCheck
1947 ''' <summary>
1948 ''' PC-PLC�ʐM�`�F�b�N
1949 ''' </summary>
1950 ''' <returns>   0 : NG
1951 '''             1 : OK(�Ǎ��݊���)
1952 ''' </returns>
1953 ''' <remarks>
1954 ''' Date   : 2021/07/07 : M.Hayakawa
1955 ''' </remarks>'
1956 Function M% fnPCComuCheck
1957     fnPCComuCheck = 0
1958     MJudge% = 0                                  '������
1959     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1960     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1961     '
1962     For MStaNo = 0 To 5
1963         '
1964         If M_In(MIN_PIAS_ComOK%) = 1 Then
1965             'PC�ʐMOK(M400)
1966             MJudge% = MOK%
1967             MStaNo = 5
1968             Break
1969         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1970             'toRBT_�ʐM�m�Ftime out
1971             MJudge% = MNG%
1972             MCommentD1001 = 15
1973             MCommentD1002 = 21
1974             MStaNo = 5
1975             Break
1976         Else
1977             'toRBT_�ʐM�m�Ftime out
1978             MJudge% = MNG%
1979             MCommentD1001 = 14
1980             MCommentD1002 = 21
1981             Break
1982         EndIf
1983     Next MStaNo
1984     '
1985     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1986     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1987     '
1988     '�G���[���
1989     If MJudge% <> MOK% Then
1990         M_20# = MClear%     '������
1991         '�G���[�����L�q
1992         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1993         'GOT KEY���͑҂�
1994         MKeyNumber = fnKEY_WAIT()
1995         '
1996         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1997             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1998             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1999             Break
2000         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
2001             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
2002             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2003             Break
2004         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
2005             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
2006             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2007             Break
2008         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
2009             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
2010             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2011             Break
2012         EndIf
2013     Else
2014         'OK�̏ꍇ
2015         fnPCComuCheck = 1
2016     EndIf
2017     Exit Function
2018 FEnd
2019 '
2020 '��fnProcessCheck
2021 ''' <summary>
2022 ''' �H�������m�F
2023 ''' </summary>
2024 ''' <returns>    1�F�H������OK     0�F�ُ�I��
2025 '''             -1�F�O�H������NG  -2�F���H����������
2026 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
2027 '''             -5�F���������G���[
2028 ''' </returns>
2029 ''' <remarks>
2030 ''' Date   : 2021/07/07 : M.Hayakawa
2031 ''' </remarks>'
2032 Function M% fnProcessCheck
2033     fnProcessCheck = 0
2034     MJudge% = MNG%      '��UNG���������Ƃ���
2035 '----------�H�������m�F----------
2036     MCommentD1001 = 0   '�R�����g������
2037     For MStaNo = 0 To 5
2038         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
2039         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
2040         '
2041         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
2042             MJudge% = MOK%
2043             fnAutoScreenComment(85)     ' AUTO���
2044             MStaNo = 5
2045             Break
2046         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
2047             MFlgLoop% = 0
2048             MJudge% = MNG%
2049             MCommentD1001 = 27
2050             MCommentD1002 = 22
2051             fnAutoScreenComment(94)     ' AUTO���
2052             fnProcessCheck = -2         ' NG��-2��Ԃ�
2053             MStaNo = 5
2054             Break
2055         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
2056            MJudge% = MNG%
2057             MCommentD1001 = 31
2058             MCommentD1002 = 22
2059             fnAutoScreenComment(83)     ' AUTO���
2060             fnProcessCheck = -3         ' NG��-3��Ԃ�
2061             MStaNo = 5
2062             Break
2063         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
2064             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
2065             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
2066             MJudge% = MNG%
2067             MCommentD1001 = 32
2068             MCommentD1002 = 22
2069             fnAutoScreenComment(84)     ' AUTO���
2070             fnProcessCheck = -1         ' NG��-1��Ԃ�
2071             Dly 1.0
2072             '�H�������m�FOFF
2073             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
2074             Dly 1.0
2075            'MStaNo = 5
2076             Break
2077         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
2078             MFlgLoop% = 0
2079             MJudge% = MNG%
2080             MCommentD1001 = 29
2081             MCommentD1002 = 22
2082             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
2083             fnProcessCheck = -5         ' NG��-5��Ԃ�
2084             MStaNo = 5
2085             Break
2086         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
2087             MJudge% = MNG%
2088             If MCommentD1001 = 32 Then
2089                 '�������Ȃ�
2090             Else
2091                 MCommentD1001 = 26
2092             EndIf
2093             MCommentD1002 = 22
2094             fnProcessCheck = -4         ' NG��-4��Ԃ�
2095             MStaNo = 5
2096             Break
2097         Else
2098             MJudge% = MNG%
2099             MCommentD1001 = 28
2100             MCommentD1002 = 22
2101         EndIf
2102     Next MStaNo
2103     '�H�������m�FOFF
2104     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
2105     '�ʉߗ���NG �H�������̏ꍇ
2106     If MJudge% = MPass% Then
2107         M_20# = MPass%
2108     EndIf
2109     '
2110     '�G���[���
2111     If MJudge% <> MOK% Then
2112         M_20# = MClear%     '������
2113         '�G���[�����L�q
2114         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2115         'GOT KEY���͑҂�
2116         MKeyNumber = fnKEY_WAIT()
2117         '
2118         Select MKeyNumber
2119             Case MAbout%        '��~��I�������ꍇ
2120                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
2121                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2122                 Break
2123             Case MNext%         '���ւ�I�������ꍇ
2124                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
2125                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2126                 Break
2127             Case MContinue%     '�p����I�������ꍇ
2128                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
2129                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2130                 Break
2131             Case MNgProcess%    'NG��I�������ꍇ
2132                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
2133                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2134                 Break
2135         End Select
2136     Else
2137         fnProcessCheck = 1  ' OK��1��Ԃ�
2138     EndIf
2139     Exit Function
2140 FEnd
2141 '
2142 '��fnPiasWrite
2143 ''' <summary>
2144 ''' Pias �g�����ʏ����ݗv��
2145 ''' </summary>
2146 '''<param name="MFlg%">
2147 '''                 MOK%(1) = �H��������OK��������
2148 '''                 MNG%(0) = �H��������NG��������
2149 '''</param>
2150 '''<returns></returns>
2151 ''' <remarks>
2152 ''' Date   : 2021/07/07 : M.Hayakawa
2153 ''' </remarks>'
2154 Function M% fnPiasWrite(ByVal MFlg%)
2155       fnPiasWrite = 0
2156 *RETRY_PIASWRITE
2157     '
2158     '�g��OK(MOK%)�̏ꍇ�@M306 ON
2159    '�g��NG(MNG%)�̏ꍇ�@M307 ON
2160     If MFlg% = MOK% Then
2161         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
2162     Else
2163         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
2164     EndIf
2165     Dly 0.1                  '�O�̂���
2166     '
2167     'Pias�֏����݊J�n M305 -> ON
2168     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
2169     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
2170     '
2171     MJudge% = MNG%
2172     '
2173     For MStaNo = 0 To 5
2174         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
2175             MJudge% = MOK%
2176             'MRet = fnAutoScreenComment(85)  'AUTO���
2177             MStaNo = 5
2178             Break
2179         '
2180         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
2181             MJudge% = MNG%
2182             'MRet = fnAutoScreenComment(85)  'AUTO���
2183            MCommentD1001 = 34
2184            MCommentD1002 = 25
2185             MStaNo = 5
2186             Break
2187         '
2188         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
2189             MJudge% = MNG%
2190             'MRet = fnAutoScreenComment(85)  'AUTO���
2191            MCommentD1001 = 35
2192            MCommentD1002 = 25
2193             MStaNo = 5
2194             Break
2195         '
2196         ElseIf M_In(11583) = 1 Then                         '�H����������time out
2197             MJudge% = MNG%
2198             'MRet = fnAutoScreenComment(85)  'AUTO���
2199            MCommentD1001 = 36
2200            MCommentD1002 = 25
2201             MStaNo = 5
2202             Break
2203         '
2204         Else
2205             MJudge% = MNG%
2206            MCommentD1001 = 42
2207            MCommentD1002 = 25
2208         '
2209         EndIf
2210         '
2211     Next MStaNo
2212     '
2213     'Pias�֏����݊J�n M305 -> OfF
2214     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
2215     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
2216     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
2217     '
2218     '
2219     '�ʉߗ���NG �H�������̏ꍇ
2220     If MJudge% = MPass% Then
2221         M_20# = MPass%
2222     EndIf
2223     '
2224    M_20# = MClear%     '������
2225     '
2226     '�G���[���
2227     If MJudge% < MOK% Then
2228     '
2229 '�c���Ă���������ł͎g�p���Ȃ����x��
2230 *RETRY_ERR_WRITE
2231         M_20# = MClear%     '������
2232         '�G���[�����L�q
2233         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2234         'GOT KEY���͑҂�
2235         MKeyNumber = fnKEY_WAIT()
2236         '
2237         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2238             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2239            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2240             Break
2241         '
2242         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2243             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2244             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2245         '
2246         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2247             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2248             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2249         '
2250         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2251             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2252            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2253             Break
2254         '
2255         EndIf
2256         '
2257         If M_20# = MClear% Then *RETRY_ERR_WRITE
2258         '
2259     EndIf
2260     '
2261     If M_20# = MContinue% Then *RETRY_PIASWRITE
2262     '
2263     fnPiasWrite = 1
2264     Exit Function
2265 FEnd
2266 '
2267 '��fnPCBNumberCheck
2268 ''' <summary>
2269 ''' Pias ��ԍ��ƍ��v��
2270 ''' </summary>
2271 '''<param name="%"></param>
2272 '''<param name="%"></param>
2273 '''<returns></returns>
2274 ''' <remarks>
2275 ''' Date   : 2021/07/07 : M.Hayakawa
2276 ''' </remarks>'
2277 Function M% fnPCBNumberCheck
2278       fnPCBNumberCheck = 0
2279     '
2280 *RETRY_PCBCHECK
2281     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
2282     'Pias�֊�ƍ��J�n M310 -> ON
2283     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
2284     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
2285     '
2286     MJudge% = MNG%
2287     '
2288     For MStaNo = 0 To 5
2289         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
2290             MJudge% = MOK%
2291             fnAutoScreenComment(96)  'AUTO���
2292             MStaNo = 5
2293             Break
2294         '
2295         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
2296             MJudge% = MNG%
2297             fnAutoScreenComment(97)  'AUTO���
2298             MCommentD1001 = 37
2299             MCommentD1002 = 25
2300             MStaNo = 5
2301             Break
2302         '
2303         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
2304             MJudge% = MNG%
2305             fnAutoScreenComment(98)  'AUTO���
2306             MCommentD1001 = 38
2307             MCommentD1002 = 25
2308             MStaNo = 5
2309             Break
2310         '
2311         ElseIf M_In(11580) = 1 Then                         'time out
2312             MJudge% = MNG%
2313             fnAutoScreenComment(99)  'AUTO���
2314             MCommentD1001 = 39
2315             MCommentD1002 = 25
2316             MStaNo = 5
2317             Break
2318         '
2319         Else
2320             MJudge% = MNG%
2321            MCommentD1001 = 41
2322            MCommentD1002 = 25
2323         '
2324         EndIf
2325         '
2326     Next MStaNo
2327     '
2328     'Pias�֊�ƍ��J�n M310 -> OfF
2329     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
2330     '
2331     '
2332     '�ʉߗ���NG �H�������̏ꍇ
2333     If MJudge% = MPass% Then
2334         M_20# = MPass%
2335     EndIf
2336     '
2337    M_20# = MClear%     '������
2338     '
2339     '�G���[���
2340     If MJudge% < MOK% Then
2341     '
2342 '�c���Ă���������ł͎g�p���Ȃ����x��
2343 *RETRY_ERR_PCBNUMBER
2344         M_20# = MClear%     '������
2345         '�G���[�����L�q
2346         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2347         'GOT KEY���͑҂�
2348         MKeyNumber = fnKEY_WAIT()
2349         '
2350         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2351             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2352             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2353             Break
2354         '
2355         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2356             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2357             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2358         '
2359         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2360             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2361             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2362         '
2363         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2364             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2365             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2366             Break
2367         '
2368         EndIf
2369         '
2370         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
2371         '
2372     EndIf
2373     '
2374     If M_20# = MContinue% Then *RETRY_PCBCHECK
2375     Exit Function
2376 FEnd
2377 '
2378 '��ScrewTight
2379 ''' <summary>
2380 ''' �˂����߂��s��(S�^�C�g)
2381 ''' </summary>
2382 '''<param name="PScrewPos()">
2383 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
2384 '''             PScrewPos(2)    �F�˂����߉��_
2385 '''             PScrewPos(10)   �F�˂����ߏI������
2386 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
2387 '''             1:6mm S�^�C�g��l�W
2388 '''             2:13mm S�^�C�g��l�W
2389 '''             3:6mm S�^�C�g���l�W
2390 '''             4:3mm S�^�C�g���l�W
2391 '''             5:6mm M�l�W
2392 '''             6:13mm S�^�C�g��l�W(�˂����߃p�����[�^�Ⴂ)
2393 '''</param>
2394 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
2395 '''<returns>����
2396 '''         0=�ُ�I���A1=����I��
2397 '''</returns>
2398 ''' <remarks>
2399 ''' Date   : 2021/07/07 : M.Hayakawa
2400 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
2401 ''' </remarks>'
2402 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
2403     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2404     ScrewTight = 0
2405     MOKNGFlg = 0
2406     Ovrd 100
2407     Fine 0.05 , P
2408     Mvs PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
2409     Select MScrewType%      '�ǂݍ��݈ʒu�ύX(1/19����)
2410         Case 1
2411             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
2412             ProgramBankSet(1,1)
2413             Break
2414         Case 2
2415             ' S�^�C�g13mm�F�v���O����2�A�o���N1�ɐݒ�
2416             ProgramBankSet(2,1)
2417             Break
2418         Case 3
2419             ' S�^�C�g���F�v���O����3�A�o���N1�ɐݒ�
2420             ProgramBankSet(3,1)
2421             Break
2422         Case 4
2423             ' S�^�C�g3mm���F�v���O����4�A�o���N1�ɐݒ�
2424             ProgramBankSet(4,1)
2425             Break
2426         Case 5
2427             ' M�l�W�F�v���O����5�A�o���N1�ɐݒ�
2428             ProgramBankSet(5,1)
2429             Break
2430         Case 6
2431             ' S�^�C�g13mm(�p�����[�^�Ⴂ):�v���O����2�o���N2�ɐݒ�
2432             ProgramBankSet(2,2)
2433             Break
2434         Default
2435             ' �v���O����1�A�o���N�Ȃ��ݒ�
2436             ProgramBankSet(0,0)
2437             Break
2438     End Select
2439     Accel 100,10
2440 '    Ovrd MOvrdA%               '10/7���ݒlNull
2441     Ovrd 60                     '�O�̂��ߌ��� ���l�ύX ��
2442     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
2443     Mvs PScrewPosition(2)
2444     ' ����Ovrd�ݒ�
2445 '    Ovrd MOvrdA%
2446     Ovrd 100
2447     Accel
2448     ' Spd�ݒ�
2449     Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
2450 '    Spd MFeedSpd
2451     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
2452     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
2453 '    Select MScrewType%      '�ǂݍ��݈ʒu�ύX(1/19����)
2454 '        Case 1
2455 '            ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
2456 '            ProgramBankSet(1,1)
2457 '            Break
2458 '        Case 2
2459 '            ' S�^�C�g13mm�F�v���O����2�A�o���N1�ɐݒ�
2460 '            ProgramBankSet(2,1)
2461 '            Break
2462 '        Case 3
2463 '            ' S�^�C�g���F�v���O����3�A�o���N1�ɐݒ�
2464 '            ProgramBankSet(3,1)
2465 '            Break
2466 '        Case 4
2467 '            ' S�^�C�g3mm���F�v���O����4�A�o���N1�ɐݒ�
2468 '            ProgramBankSet(4,1)
2469 '            Break
2470 '        Case 5
2471 '            ' M�l�W�F�v���O����5�A�o���N1�ɐݒ�
2472 '            ProgramBankSet(5,1)
2473 '            Break
2474 '        Default
2475 '            ' �v���O����1�A�o���N�Ȃ��ݒ�
2476 '            ProgramBankSet(0,0)
2477 '            Break
2478 '    End Select
2479 '
2480 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
2481      '�h���C�o�[ON�@CW
2482     M_Out(12241)=1
2483     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2484     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
2485     Dly 0.1
2486     Spd M_NSpd
2487     Fine 0 , P
2488     '
2489     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
2490         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2491         Dly 0.1
2492        ' �v���O�����E�o���N����
2493         ProgramBankSet(0,0)
2494         '�p���b�g��˂����ߏI���ʒu���ֈړ�
2495         Mvs PScrewPosition(10),-80
2496         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
2497         M_Out(12249)=1 Dly 0.3
2498         MOKNGFlg = -1
2499         ScrewTight = 0
2500     Else
2501          '�h���C�o�[OFF�@CW
2502         M_Out(12241)=0
2503 '        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
2504         Select MScrewType%
2505             Case 1
2506                 ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
2507                 ProgramBankSet(1,3)
2508                 Break
2509             Case 2
2510                 ' S�^�C�g13mm�F�v���O����2�A�o���N3�ɐݒ�
2511                 ProgramBankSet(2,3)
2512                 Break
2513             Case 3
2514                 ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
2515                 ProgramBankSet(3,3)
2516                 Break
2517             Case 4
2518                 ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
2519                 ProgramBankSet(4,3)
2520                 Break
2521             Case 5
2522                 ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
2523                 ProgramBankSet(5,3)
2524                 Break
2525             Default
2526                 ' �v���O����1�A�o���N�Ȃ��ݒ�
2527                 ProgramBankSet(0,0)
2528                 Break
2529         End Select
2530          '�h���C�o�[ON�@CW
2531         Mvs PScrewPosition(10)
2532         M_Out(12241)=1
2533         Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2534 '
2535          '�h���C�o�[OFF�@CW
2536         M_Out(12241)=0
2537        ' �v���O�����E�o���N����
2538         ProgramBankSet(0,0)
2539         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
2540         M_Out(12249)=1 Dly 0.3
2541     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
2542         '�p���b�g��˂����ߏI���ʒu���ֈړ�
2543         Mvs PScrewPosition(10),-80
2544         ScrewTight = 1
2545     EndIf
2546 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
2547 '    Ovrd 10
2548 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2549     Ovrd 100
2550     Exit Function
2551 FEnd
2552 '
2553 '��ScrewGet
2554 ''' <summary>
2555 ''' �˂������@����˂��𓾂�
2556 ''' </summary>
2557 '''<param name="%">
2558 '''         PScrewPos(1)    �F�˂�������̂˂����
2559 '''         PScrewPos(2)    �F�˂���������_
2560 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
2561 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2562 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2563 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2564 '''</param>
2565 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
2566 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
2567 '''<returns>����
2568 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
2569 '''</returns>
2570 ''' <remarks>
2571 ''' Date   : 2021/07/07 : M.Hayakawa
2572 ''' </remarks>
2573 '''<update>
2574 '''Date    : 2021/11/15 : ����
2575 '''</update>
2576 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
2577     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
2578     ScrewGet = 0
2579     MScrewJudge% = 0
2580     '�˂������평������G���[�`�F�b�N
2581 ' ���b��폜
2582     Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
2583     For MCnt% = 0 To MFinCnt%
2584        '�˂������평������G���[�`�F�b�N
2585         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
2586         If MRtn = 0 Then
2587             'Ovrd 30
2588             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
2589             ScrewGet = -1
2590             MScrewJudge% = 2
2591         EndIf
2592         Ovrd 100
2593         If FeederScrewSensor% <> 0 Then
2594             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
2595                 'Ovrd 30
2596                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
2597                 'NG�Ƃ��Ă����̊֐����甲����
2598                 ScrewGet = -2
2599                 MScrewJudge% = 3
2600             EndIf
2601         EndIf
2602         Ovrd 100
2603         Spd M_NSpd
2604         '�˂������J�n
2605         If MScrewJudge% = 0 Then
2606     '        ScrewGet = 0
2607             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2608             MScrewCnt% = 0
2609             MFinCnt% = 2
2610             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2611             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2612             'Ovrd 5 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
2613             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2614             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2615             Mvs PScrewPosition(10), 1.2
2616             M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
2617             '�r�b�g��]
2618             M_Out(Y60_Driver)=1
2619             Dly 0.2
2620             '
2621             'Ovrd 5 '2�ɕύX 10/6 M.H '5�ɕύX10/7�����@�폜��
2622             JOvrd M_NJovrd
2623             Spd M_NSpd
2624             '�l�W�z���m�F�ʒu�ړ�
2625             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2626             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2627             '�r�b�g��]��~
2628             M_Out(Y60_Driver)=0
2629             '
2630             '1�b�ԃl�W�z���m�F
2631             MRtn = frInCheck(11268, 1, MSETTIMEOUT01&)
2632             'MRtn = 0'�����G���[
2633             '�z���G���[�̏ꍇ
2634             '�l�W���˂����Y�ɖ߂�
2635             If MRtn = 0 Then
2636                 Ovrd 5      '2����5�ɕύX
2637                 '�r�b�g��]��~
2638                 M_Out(Y60_Driver)=0
2639                 '�l�W�����@���
2640                 Mvs PScrewPosition(1)
2641                 '�X�ɏ��
2642                 Mov PScrewPosition(1), -140
2643                 '�l�W�̂Ĉʒu
2644                 If FeederReadyNo% = 11259 Then     '�����@�ʂɋz���G���[�����J�E���g�@2022/05/19 �n��
2645                     MRtn = FnCtlValue2(3)          '�����@�Q�z���G���[���{�P
2646                 Else
2647                     MRtn = FnCtlValue2(4)          '�����@�P�z���G���[���{�P  2022/04/28 �n��
2648                 EndIf
2649                 Mov PScrewPosition(9)
2650                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
2651                 '�z��OFF
2652                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
2653                 Dly 0.2
2654                 '�j��ON
2655                 M_Out(Y6B_VB1)=1 '�^��j��ON
2656                 '�r�b�g��]
2657                 M_Out(Y61_Driver)=1
2658                 Dly 0.5
2659                 '                '
2660                 Ovrd 100
2661                 JOvrd M_NJovrd
2662                 Spd M_NSpd
2663                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2664                 Mov PScrewPosition(9), 10
2665                 Mov PScrewPosition(9)
2666                 Dly 0.1
2667                 Mov PScrewPosition(9), 10
2668                 Mov PScrewPosition(9)
2669                 '
2670                 '�l�W�����҂�
2671                 Wait M_In(11268) = 0
2672                 '�r�b�g��]��~
2673                 M_Out(Y61_Driver)=0
2674                 Dly 0.1
2675                 '�j��OFF
2676                 M_Out(Y6B_VB1)=0 '�^��j��OFF
2677                 '�˂��������Ƃ��āA�ړ��X�ɏ��
2678                 Mov PScrewPosition(1), -140
2679                 Ovrd 100
2680                 Spd M_NSpd
2681                 '�l�W�����@���
2682                 Mvs PScrewPosition(1)
2683 '                '
2684                 ScrewGet = -3
2685                 Break
2686 '                '
2687             Else
2688                 MCnt% = MFinCnt%
2689                 ScrewGet = 0
2690             EndIf
2691         Else
2692             MCnt% =MFinCnt%
2693         EndIf
2694     Next  MCnt%
2695         '
2696     If MScrewJudge% = 0 Then
2697         Ovrd 100
2698         Spd M_NSpd
2699         Mvs PScrewPosition(10), -20  ' �˂��s�b�N�A�b�v�ʒu -20mm
2700         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2701         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2702         Mvs PScrewPosition(10), -20  ' �˂��s�b�N�A�b�v�ʒu -20mm
2703         Mov PScrewPosition(2)
2704         '������x�z���m�F
2705         MRtn = frInCheck(11268, 1, MSETTIMEOUT01&)
2706         If MRtn = 0 Then      '�z���G���[�̏ꍇ
2707             MScrewJudge% = 4
2708             ScrewGet = -3
2709         ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
2710             MScrewJudge% = 1
2711             ScrewGet = 1
2712         EndIf
2713         Break
2714     EndIf
2715     If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
2716     '
2717     Select MScrewJudge%
2718         Case 0
2719 '            fErrorProcess(11,162,163,0) '�ُ�I��
2720             MCommentD1001 = 162
2721             MCommentD1002 = 96
2722             Break
2723         Case 2
2724 '            fErrorProcess(11,63,161,0) '����NG
2725             MCommentD1001 = 63
2726             MCommentD1002 = 96
2727             Break
2728         Case 3
2729 '            fErrorProcess(11,160,164,0) '�닟��
2730             MCommentD1001 = 237
2731             MCommentD1002 = 96
2732             Break
2733         Case 4
2734 '            fErrorProcess(11,94,95,0) '�z��NG
2735             MCommentD1001 = 94
2736             MCommentD1002 = 95
2737             Break
2738     End Select
2739     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
2740     '
2741     Select M_20#
2742         Case MAbout%          '��~�������ꂽ�ꍇ
2743             Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����
2744 '            Mov PInitialPosition
2745             Break
2746         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
2747             Break
2748         Case MNext%           '�p���������ꂽ�ꍇ
2749             M_20# = MClear%     '������
2750             Break
2751         Case MNgProcess%      'NG�������ꂽ�ꍇ
2752             Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��
2753 '            Mov PInitialPosition
2754             Break
2755         End Select
2756 *End_ScrewGet
2757     Exit Function
2758 FEnd
2759 '
2760 '��ProgramBankSet
2761 ''' <summary>
2762 ''' �˂����߂��s��(P�^�C�g)
2763 ''' </summary>
2764 '''<param name="MProgramNo">�v���O�����ԍ�</param>
2765 '''<param name="MBankNo">�o���N�ԍ�</param>
2766 '''</returns>
2767 ''' <remarks>
2768 ''' Date   : 2021/10/05 : M.Hayakawa
2769 ''' </remarks>'
2770 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
2771 '
2772     MLocalPrgNo% = (MProgramNo% - 1) * 32
2773     MLocalBankNo% = MBankNo% * 4
2774 '
2775     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
2776         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
2777     Else
2778         MLocalOutNo% = 0
2779     EndIf
2780 '
2781     M_Out8(12240) = MLocalOutNo%
2782     Dly 0.1
2783     Exit Function
2784 FEnd
2785 '
2786 '��fnKEY_WAIT()
2787 ''' <summary>
2788 ''' GOT����̃L�[���͑҂�
2789 ''' </summary>
2790 '''<returns>1�F��~    2�F����
2791 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2792 '''         5�FNG
2793 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2794 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2795 '''</returns>
2796 ''' <remarks>
2797 ''' Date   : 2021/07/07 : M.Hayakawa
2798 ''' </remarks>'
2799 Function M% fnKEY_WAIT()
2800     fnKEY_WAIT = 0
2801     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2802     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2803     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2804     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2805     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2806     Dly 0.2
2807     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2808     MLocalLoopFlg=1
2809     While MLocalLoopFlg=1
2810         If M_In(11345) = 1 Then         '��~   M5345
2811             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2812             fnKEY_WAIT = 1
2813             MLocalLoopFlg=-1
2814             Break
2815         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2816             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2817             fnKEY_WAIT = 2
2818             MLocalLoopFlg=-1
2819             Break
2820         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2821             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2822             fnKEY_WAIT = 3
2823             MLocalLoopFlg=-1
2824             Break
2825         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2826             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2827             fnKEY_WAIT = 4
2828             MLocalLoopFlg=-1
2829             Break
2830         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2831             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2832             fnKEY_WAIT = 5
2833             MLocalLoopFlg=-1
2834             Break
2835             '
2836         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2837             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2838             fnKEY_WAIT = MRobotInit1%
2839             MLocalLoopFlg=-1
2840             Break
2841             '
2842         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2843             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2844             fnKEY_WAIT = MRobotInit2%
2845             MLocalLoopFlg=-1
2846             Break
2847             '
2848         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2849             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2850             fnKEY_WAIT = MRobotInit3%
2851             MLocalLoopFlg=-1
2852             Break
2853             '
2854         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2855             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2856             fnKEY_WAIT = MRobotInit4%
2857             MLocalLoopFlg=-1
2858             Break
2859             '
2860         Else
2861         EndIf
2862     WEnd
2863     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2864     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2865     Exit Function
2866 FEnd
2867 '
2868 '�� fnAUTO_CTL
2869 ''' <summary>
2870 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2871 ''' </summary>
2872 ''' <remarks>
2873 ''' Date   : 2021/07/07 : M.Hayakawa
2874 ''' </remarks>
2875 Function M% fnAUTO_CTL
2876     fnAUTO_CTL = 0
2877     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2878     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2879     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2880     '
2881     If M_Svo=0 Then             '�T�[�{ON�m�F
2882         Servo On
2883     EndIf
2884     Wait M_Svo=1
2885     Exit Function
2886 FEnd
2887 '
2888 '�� fnWindScreenOpen
2889 ''' <summary>
2890 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2891 ''' </summary>
2892 '''<param name="%"></param>
2893 '''<param name="%"></param>
2894 '''<param name="%"></param>
2895 '''<param name="%"></param>
2896 ''' <remarks>
2897 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2898 ''' MWindReSet = 0     ��ʔ�\��
2899 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2900 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2901 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2902 ''' Date   : 2021/07/07 : M.Hayakawa
2903 ''' </remarks>
2904 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2905     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2906         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2907     EndIf
2908     '
2909     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2910         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2911     EndIf
2912     '
2913     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2914        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2915     EndIf
2916     '
2917     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2918     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2919     Dly 0.5
2920     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2921     Exit Function
2922 FEnd
2923 '
2924 '��FnCtlValue2
2925 ''' <summary>
2926 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2927 ''' </summary>
2928 ''' <param name="MCtlNo%"></param>
2929 ''' <remarks>
2930 ''' Date : 2022/04/28 �n��
2931 ''' </remarks>
2932 '''
2933 '''  1�F������       �{�P
2934 '''  2�F�g���n�j��   �{�P
2935 '''  3�F�����@�Q�z���G���[�� �{�P�@�@�g��NG����ύX 2022/05/19 �n��
2936 '''  4�F�����@�P�z���G���[�� �{�P
2937 ''' 99�F�Ǐ��J�n�M�� OFF
2938 '''
2939 Function M% FnCtlValue2(ByVal MCtlNo%)
2940     FnCtlValue2 = 1
2941     Select MCtlNo%
2942         Case 1        '�������{�P
2943             M_Out(12569) = 0             '�����݊J�n�M��OFF
2944             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2945             MInputQty = M_In16(11600)    '��������M
2946             MInputQty = MInputQty + 1    '�������{�P
2947             M_Out16(12592) = MInputQty   '���������M
2948             M_Out(12569) = 1             '�����݊J�n�M��ON
2949             Break
2950             '
2951         Case 2        '�g���n�j���{�P
2952             M_Out(12569) = 0             '�����݊J�n�M��OFF
2953             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2954             MAssyOkQty = M_In16(11616)   '�g��OK����M
2955             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2956             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2957             M_Out(12569) = 1             '�����݊J�n�M��ON
2958             Break
2959             '
2960         Case 3        '�����@�Q�z���G���[���{�P
2961             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2962             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2963             MSuctionErrQty = M_In16(11632)         '�����@�Q�z���G���[����M
2964             MSuctionErrQty = MSuctionErrQty + 1    '�����@�Q�z���G���[���{�P
2965             M_Out16(12624) = MSuctionErrQty        '�����@�Q�z���G���[�����M
2966             M_Out(12569) = 1                       '�����݊J�n�M��ON
2967             Break
2968             '
2969         Case 4        '�����@�P�z���G���[���{�P
2970             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2971             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2972             MSuctionErrQty = M_In16(11648)         '�����@�P�z���G���[����M
2973             MSuctionErrQty = MSuctionErrQty + 1    '�����@�P�z���G���[���{�P
2974             M_Out16(12640) = MSuctionErrQty        '�����@�P�z���G���[�����M
2975             M_Out(12569) = 1                       '�����݊J�n�M��ON
2976             Break
2977             '
2978         Case 99        '�Ǐ��J�n�M��OFF
2979             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2980             M_Out(12569) = 0        '�����݊J�n�M��OFF
2981             Break
2982             '
2983     End Select
2984     Exit Function
2985 FEnd
2986 '
2987 '
2988 '��FnScreEroorCord
2989 ''' �d���h���C�o�[�̃G���[�R�[�h���܂߂��R�����g���o���ׂ̃R�����g�ԍ��̍쐬
2990 ''' �V�K�쐬�F2022/05/23 : �n��
2991 '''
2992 Function M% FnScreEroorCord()
2993     MScrewErrorCord% = 0
2994     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2995     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2996     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2997     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2998     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2999     MScrewErrorCord% = MScrewErrorCord% * 10
3000     MScrewErrorCord% = MScrewErrorCord% + 500
3001     FnScreEroorCord = MScrewErrorCord%
3002     Exit Function
3003 FEnd
3004 '
3005 '
3006 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
3007 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
3008 '-------------------------------------------------------------------------------
3009 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
3010 '   ����
3011 '       PInspPos()      �F�����ʒu
3012 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
3013 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
3014 '       MInspCnt%       �F�����ʒu��
3015 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
3016 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
3017 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
3018 '   �߂�l�F����
3019 '       0=�ُ�I���A1=����I��
3020 '
3021 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
3022 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
3023 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
3024 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
3025 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
3026 '-------------------------------------------------------------------------------
3027     '----- �����ݒ� -----
3028     Cnt 0                                                           '�ړ�����������(�����l=0)
3029     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
3030 '    Cnt 1,0.1,0.1
3031     '�ϐ��錾�E������
3032     Def Inte MNum                                                   '�����ԍ�(������1�`)
3033     MNum% = 1                                                       '�����ԍ������l�ݒ�
3034     Def Inte MEndFlg                                                '�����I���t���O
3035     MEndFlg% = 0
3036     '
3037     '����G�ԍ��ݒ�v���E�������s�v��off
3038     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
3039     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
3040     '�G���[�ԍ��N���A
3041     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
3042     M_Out16(MOUT_InspErrNum) = MInspErrNum
3043     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
3044     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
3045     '
3046     'Insight Ready check?
3047     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
3048         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
3049         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
3050         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
3051         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
3052         Exit Function
3053     EndIf
3054     '
3055     '�����ʒu���m�F
3056     If MInspCnt% < 1 Or 30 < MInspCnt% Then
3057         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
3058         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
3059         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
3060         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
3061         Exit Function
3062     EndIf
3063     '
3064     '
3065     '
3066     '----- ���C������ -----
3067     '�ݒ肳�ꂽ�����ʒu�����̌������s
3068     While( MEndFlg% = 0 )
3069         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
3070         MSetGrNumRetryExitFlg = 0
3071         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
3072         While( MSetGrNumRetryExitFlg = 0 )
3073         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
3074             '
3075             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
3076             '
3077             '----- �����O���[�v�ԍ��ݒ� -----
3078             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
3079             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
3080             '
3081             '�����ʒu�ֈړ��E�ړ������҂�
3082             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
3083             Mvs PInspPos( MNum% )                                       '�ړ�
3084             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
3085             Dly 0.05                                                    '�ړ�������Delay
3086             '
3087             '�����O���[�v�ԍ��ݒ�I���m�F
3088             M_Timer(1) = 0
3089             MExitFlg = 0
3090             While( MExitFlg = 0 )
3091                 '����G�ݒ萳��I��?
3092                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
3093                     MExitFlg = 1
3094                 '
3095                 '����G�ݒ�ُ�I��?
3096                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
3097                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
3098                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
3099                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
3100                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
3101                     EndIf
3102                     MExitFlg = 1
3103                 '
3104                 'timeout�`�F�b�N
3105                 ElseIf 1000 < M_Timer(1) Then
3106                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
3107                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
3108                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
3109                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
3110                     EndIf
3111                     MExitFlg = 1
3112                 EndIf
3113             WEnd
3114             '
3115             '����G�ԍ��ݒ�v��off
3116             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
3117             '
3118             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
3119             'NG�Ȃ���Δ�����
3120             If MCurrentStepErr = 0 Then
3121                 MSetGrNumRetryExitFlg = 1
3122             Else
3123                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
3124                 If MSetGrNumRetryCnt = 0 Then
3125                     MSetGrNumRetryExitFlg = 1
3126                 Else
3127                     'Retry�ց@���̑O��Delay
3128                     Dly 0.5
3129                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
3130                 EndIf
3131             EndIf
3132             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
3133             '
3134         WEnd
3135         '
3136         '
3137         '
3138         '----- �������s -----
3139         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
3140             If 0 < MInspGrNum%(MNum%) Then                          '��������?
3141                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
3142                 MInspRetryExitFlg = 0
3143                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
3144                 While( MInspRetryExitFlg = 0 )
3145                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
3146                     '
3147                     '���������m�F
3148                     MRetryCnt = MRetryCnt - 1
3149                     M_Timer(1) = 0
3150                     MExitFlg = 0
3151                     While( MExitFlg = 0 )
3152                     '���������҂�
3153                         '����OK�I��?
3154                         If M_In( MIN_IS_InspOK% ) = 1  Then
3155                             MJudgeOKFlg = 1                         '����OK�t���OON
3156                             MExitFlg = 1
3157                         '
3158                         '����NG�I��?
3159                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
3160                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
3161                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
3162                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
3163                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
3164                                 EndIf
3165                             EndIf
3166                             MExitFlg = 1
3167                         '
3168                         '�����ُ�I��(IS timeout)?
3169                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
3170                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
3171                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
3172                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
3173                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
3174                                 EndIf
3175                             EndIf
3176                             MExitFlg = 1
3177                         '
3178                         'timeout�`�F�b�N
3179                         ElseIf 3000 < M_Timer(1) Then
3180                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
3181                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
3182                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
3183                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
3184                                 EndIf
3185                             EndIf
3186                             MExitFlg = 1
3187                         EndIf
3188                     WEnd
3189                     '
3190                     '�����J�n�v��off
3191                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
3192                     '
3193                     'OK�Ȃ甲����
3194                     If MJudgeOKFlg = 1 Then
3195                         MInspRetryExitFlg = 1
3196                     Else
3197                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
3198                         If MRetryCnt = 0 Then
3199                             MInspRetryExitFlg = 1
3200                         Else
3201                             'Retry�ց@���̑O��Delay
3202                             Dly 0.3
3203                         EndIf
3204                     EndIf
3205                     '
3206                 WEnd
3207             EndIf
3208         EndIf
3209         '
3210         '
3211         '
3212         MNum% = MNum% + 1                                           '����Step+1
3213         '�����I���m�F�@�����I���t���O�Z�b�g
3214         If (MInspCnt% < MNum% ) Then
3215             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
3216         EndIf
3217         'NG���������s������
3218         If MInspErrNum <> 0 Then                                    'NG����?
3219             If MNgContinue% <> 1 Then                               'NG���s?
3220                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
3221             EndIf
3222         EndIf
3223     WEnd
3224     '
3225     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
3226     If 0 < MZAxis% Then
3227         PCurrentPos = P_Curr                                        '���݈ʒu�擾
3228         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
3229         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
3230         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
3231     EndIf
3232     '
3233     '�߂�l�ݒ�
3234     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
3235         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
3236     Else
3237         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
3238         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
3239         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
3240     EndIf
3241     Fine 0 , P
3242     Exit Function
3243 FEnd
3244 '
3245 '��InitialZoneB
3246 ''' <summary>
3247 ''' ����~��̕��A����
3248 ''' 1)���ޔ��@Z������Ɉړ�
3249 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
3250 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
3251 ''' 4)�C�j�V�����|�W�V�����ֈړ�
3252 ''' </summary>
3253 ''' <remarks>
3254 ''' Date : 2022/03/23 : N.Watanabe
3255 ''' </remarks>
3256 Function V fnInitialZoneB()
3257     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/05/09 �n��
3258 '�p�����[�^
3259     Ovrd 5
3260 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
3261 '    Cmp Pos, &B100011
3262 '
3263 '���A����J�n
3264 '�n���h���C�j�V�����ɖ߂�
3265 '    M_Out(12256) = 0    '�{�̃`���b�N��OFF
3266 '    M_Out(12257) = 1    '�{�̃`���b�N�JON
3267 '    Dly 1.0
3268 '���ޔ�
3269     PActive = P_Curr
3270     Pmove = PActive
3271     Pmove.Z = 600           '���ޔ�����ꗥ�̍���
3272     If PActive.X > 400 Then
3273         Pmove.Z =400        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
3274     EndIf
3275     If PActive.Z < Pmove.Z Then '���݂̍�����Pmove���Ⴂ���̂ݎ��s
3276         Mvs Pmove
3277     EndIf
3278     Dly 1.0
3279 'J1���ȊO��ޔ��|�W�V�����ֈړ�
3280     JActive = J_Curr
3281     Jmove = JTaihi
3282     Jmove.J1 = JActive.J1        'J1���̂݌��ݒl���g�p���A���̎���JTaihi�̃|�[�Y�����
3283     Mov Jmove
3284     Dly 1.0
3285 'J1���݂̂�ޔ��|�W�V�����ֈړ�
3286     Mov JTaihi
3287     Dly 1.0
3288 '�C�j�V�����|�W�V�����ֈړ�
3289     Mov PInitialPosition
3290     Cmp Off
3291     Ovrd 100
3292     M_Out(12268) = 0            '�ʒu���ߏoOFF
3293     M_Out(12269) = 1            '�ʒu���ߖ�ON
3294     fErrorProcess(11,253,281,0)
3295     Exit Function
3296 FEnd
3297 '
3298 '
3299 '��fnAutoScreenComment
3300 ''' <summary>
3301 ''' ���C����ʂ̓���󋵕\��
3302 ''' �R�����gD1005�̐ݒ�
3303 ''' </summary>
3304 '''<param name="McommentD1005%">�R�����gID</param>
3305 ''' <remarks>
3306 ''' Date   : 2021/07/07 : M.Hayakawa
3307 ''' </remarks>
3308 Function fnAutoScreenComment(ByVal McommentD1005%)
3309     M_Out16(12576) = McommentD1005%
3310     Exit Function
3311 FEnd
3312 '
3313 '��fnRoboPosChk
3314 ''' <summary>
3315 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
3316 ''' </summary>
3317 '''<param name="MINNumber%">���͔ԍ�</param>
3318 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3319 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3320 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
3321 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
3322 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3323 ''' <remarks>
3324 ''' Date   : 2021/07/07 : M.Hayakawa
3325 ''' </remarks>
3326 Function M% fnRoboPosChk
3327     fnRoboPosChk = 0
3328     MRet = fnStepRead()
3329     '�����ʒu�łȂ��Ɣ��f�����ꍇ
3330     '�E�B���h��ʐ؊���
3331     If MRBTOpeGroupNo > 5 Then
3332         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3333         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3334         Dly 0.2
3335         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3336         Dly 1.5
3337         '
3338         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3339         '
3340         MLoopFlg% = 1
3341         While MLoopFlg% = 1
3342             '
3343             '
3344             MKeyNumber% = fnKEY_WAIT()
3345             Select MKeyNumber%
3346                 Case Is = MAbout%       '��~
3347                     M_20# = MAbout%
3348                     MLoopFlg% = -1
3349                     Break
3350                 Case Is = MNext%        '����
3351                     'MLoopFlg% = -1
3352                     Break
3353                 Case Is = MContinue%    '�p��
3354                     M_20# = MContinue%
3355                     MLoopFlg% = -1
3356                     Break
3357                 Default
3358                     Break
3359             End Select
3360         WEnd
3361     EndIf
3362     '
3363     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3364         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3365         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3366         Select MRBTOpeGroupNo
3367             Case Is = 5                          '�������Ȃ�
3368                 Break
3369             Case Is = 10                         '�����ʒu�֖߂�
3370                 'Mov PTEST001
3371                 Break
3372             Case Is = 15                         '�����ʒu�֖߂�
3373                 'Mov PTEST002
3374                 Dly 0.5
3375                 'Mov PTEST001
3376                 Dly 0.5
3377                 Break
3378             Default
3379                 Break
3380         End Select
3381         '
3382         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3383         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3384         MRBTOpeGroupNo = 5
3385         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3386         Dly 1.0
3387         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3388         fnRoboPosChk = 1                        '�����ʒu������s
3389         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3390     EndIf
3391     Exit Function
3392 FEnd
3393 '
3394 '��frInCheck
3395 ''' <summary>
3396 ''' �Z���T�[IN�`�F�b�N
3397 ''' </summary>
3398 '''<param name="MINNumber%">���͔ԍ�</param>
3399 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3400 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3401 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3402 ''' <remarks>
3403 ''' Date   : 2021/07/07 : M.Hayakawa
3404 ''' </remarks>
3405 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3406     M_Timer(4) = 0
3407     MloopFlg = 0
3408     While MloopFlg = 0
3409         MCrtTime& = M_Timer(4)
3410         If M_In(MINNumber%) = MCMPFLG% Then
3411             MloopFlg = 1
3412             frInCheck = 1
3413         ElseIf MCrtTime& > MTimeCnt& Then
3414             MloopFlg = 1
3415             frInCheck = 0
3416         EndIf
3417     WEnd
3418     Exit Function
3419 FEnd
3420 '-----------------------------------------------
3421 '
3422 '�˂����ߋ@�ʐM�m�F
3423 '
3424 '-----------------------------------------------
3425 Function M% fScewTcomChk
3426     fScewTcomChk = 0
3427     '�ʐM�m�F���M
3428     M_Out(MOUT_ScwT_ComChk%) = MOn%
3429     '�ʐM�m�F��M�ҋ@
3430     Wait M_In(MIN_ScwT_comOK%) = MOn%
3431     '�ʐM�m�F���M�I��
3432     M_Out(MOUT_ScwT_ComChk%) = MOff%
3433     Exit Function
3434 FEnd
3435 '
3436 '
3437 '-----------------------------------------------
3438 '
3439 '�˂����ߊJ�n���M
3440 '
3441 '-----------------------------------------------
3442 Function M% fScewTStart
3443     fScewTStart = 0
3444     '�˂����ߊJ�n�ҋ@����M
3445     Wait M_In(MIN_ScwT_STRec%) = MOn%
3446     Dly 0.1
3447     '�˂����ߊJ�n��M�𑗐M
3448     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
3449     Exit Function
3450 FEnd
3451 '
3452 '
3453 '-----------------------------------------------
3454 '
3455 '�˂����ߊ�����M
3456 '
3457 '-----------------------------------------------
3458 Function M% fScewTFinish
3459     fScewTFinish = 0
3460     '�˂����ߊ����ҋ@����M
3461     Wait M_In(MIN_ScwT_Fin%) = MOn%
3462     Dly 0.1
3463     '�˂����ߊ�����M�𑗐M
3464     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
3465     Exit Function
3466 FEnd
3467 '
3468 '
3469 '-----------------------------------------------
3470 '
3471 '����xx��~��M
3472 '
3473 '-----------------------------------------------
3474 Function M% fScewTCaseStop(ByVal MCase%())
3475     fScewTCaseStop = 0
3476     '����xx��~����M
3477     Wait M_In(MCase%(1)) = MOn%
3478     Dly 0.1
3479     '����xx��~��M�𑗐M
3480     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
3481     Exit Function
3482 FEnd
3483 '
3484 '-----------------------------------------------
3485 '
3486 '�ĊJ�n��M
3487 '
3488 '-----------------------------------------------
3489 Function M% fScewTReStart()
3490     fScewTReStart = 0
3491     '�ĊJ�n����M
3492     Wait M_In(MIN_ScwT_ReST%) = MOn%
3493     Dly 0.1
3494     '�ĊJ�n��M�𑗐M
3495     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
3496     Exit Function
3497 FEnd
3498 '
3499 '��fErrorProcess
3500 '<summary>
3501 '�G���[����
3502 '</summary>
3503 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3504 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3505 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3506 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3507 '<make>
3508 '2021/11/5 �����V��
3509 '</make>
3510 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3511     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3512     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3513     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3514     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3515 *RETRY_ERR_PROCESS
3516      M_20# = MClear%     '������
3517 '        '�G���[�����L�q
3518         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3519 '        'GOT KEY���͑҂�
3520         MKeyNumber = fnKEY_WAIT()
3521 '        '
3522         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3523             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3524             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3525             Break
3526          '
3527         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3528             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3529             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3530         '
3531         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3532             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3533             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3534          '
3535         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3536             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3537             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3538             Break
3539         '
3540         EndIf
3541         '
3542         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3543     Exit Function
3544 FEnd
3545 '
3546 '��fnTorqueCheck
3547 ''' <summary>
3548 ''' �g���N�`�F�b�N����p�̃��C��
3549 ''' </summary>
3550 ''' <remarks>
3551 ''' Date   : 2021/12/21 : H.AJI
3552 ''' </remarks>'
3553 Function M% fnTorqueCheck
3554     '�g���N�`�F�b�N�����M  �����n��~
3555     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3556     '
3557     fnTorqueCheck = 0
3558     Ovrd 20
3559     Mov PInitialPosition              '�����ʒu�ړ�
3560     Ovrd 100
3561     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3562     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3563     Dly 0.2
3564     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3565     '
3566     'M6340  �g���N�`�F�b�N��M
3567     'Dly 5.0
3568     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3569     Dly 1.0
3570     M_Out(12340) = 0
3571     '
3572     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3573     '
3574     MLoopFlg = 1
3575     While MLoopFlg = 1
3576         '
3577         Mov PInitialPosition              '�����ʒu�ړ�
3578         '
3579         MKeyNumber = fnKEY_WAIT()
3580         Select MKeyNumber
3581             Case Is = 1           '��~
3582                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3583                 Dly 1.0
3584                 M_Out(12343) = 0
3585                 Ovrd 20
3586                 Mov PTicketRead_1
3587                 Ovrd 100
3588                 M_20# = 1
3589                 MLoopFlg = -1
3590                 Break
3591             Case Is = 2           '����
3592                 Break
3593             Case Is = 3           '�p��
3594                 Break
3595             Case Is = 4           '�g���N�`�F�b�N�J�n
3596                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
3597                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
3598                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3599                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3600                 MRet = fnMoveTorquePosi()
3601                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3602                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3603                 Break
3604             Default
3605                 Break
3606         End Select
3607     WEnd
3608     '
3609     '�g���N�`�F�b�N����~���M
3610     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3611     '
3612     '���{�b�g�̈ʒu�����ɖ߂�
3613     '
3614     Exit Function
3615  FEnd
3616  '
3617 '
3618 '
3619 '---------------------------
3620 '
3621 '    ���C����ʂ̕\���A��\���ݒ�
3622 '         �R�����gD1001, D1002, D1003�̐ݒ�
3623 '           MWindReSet = 0     ��ʔ�\��
3624 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3625 '           MWindErrScr = 10    �G���[��� D1001, D1002
3626 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3627 '
3628 '---------------------------
3629 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3630     fnMainScreenOpen = 0
3631     '
3632    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3633         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3634     EndIf
3635     '
3636     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3637         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3638     EndIf
3639     '
3640     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3641         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3642     EndIf
3643     '
3644     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3645     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3646     Dly 0.5
3647     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3648     Exit Function
3649 FEnd
3650 '
3651 '��Main
3652 ''' <summary>
3653 ''' �g���N�`�F�b�N������
3654 ''' </summary>
3655 ''' <remarks>
3656 ''' Date   : 2021/12/21 : H.AJI
3657 ''' </remarks>'
3658 Function M% fnMoveTorquePosi
3659      fnMoveTorquePosi = 0
3660      Ovrd 50
3661      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
3662     '
3663     Spd M_NSpd
3664 '-------------      �h���C�o�[RST
3665     M_Out(12240)=0     '�h���C�o�[OFF CCW
3666     M_Out(12241)=0     '�h���C�o�[OFF CW
3667     M_Out(12242)=0     '�h���C�o�[���� C1
3668     M_Out(12243)=0     '�h���C�o�[���� C2
3669     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
3670 '---------------------------------------
3671 '[P-11]
3672 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
3673     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
3674     Dly 0.1
3675 '-----------------------
3676    'Cnt 0                           'Cnt����-2�@�I��
3677 '-----------------------
3678     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
3679     Dly 0.2
3680 '-----------------------
3681     ProgramBankSet(1,3)
3682     M_Out(12241)=0                   '�h���C�o�[OFF  CW
3683     'Dly 0.1
3684 '--------------------------------
3685     Ovrd 40
3686    'Dly 0.1
3687 '--------------------------------  �l�W���ߑ��x�ݒ�
3688     Spd 14                            '���C�h 100-40 100% :Spd 12
3689     Dly 0.1
3690 '--------------------------------
3691 '--------------------------------
3692 '---------------------------------�y�˂����ߓ���z
3693 '
3694     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
3695    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
3696     Dly 0.3                          '�������҂�
3697    M_Out(12241)=1                   '�h���C�o�[ON  CW
3698 '
3699     Wait M_In(11584)=1                '����/�G���[���o
3700     Dly 0.1
3701     Spd M_NSpd
3702    'Ovrd 20
3703     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
3704     Wait M_In(11257)=1                '�l�W����SC
3705 '---------------------------------
3706     Dly 0.1
3707     M_Out(12241)=0                    '�h���C�o�[OFF CW
3708     Dly 0.1
3709     M_Out(12242)=0                    '�h���C�o�[���� C1
3710     Dly 0.1
3711     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
3712     Dly 0.1
3713     M_Out(12245)=0                    '�v���O����2���� F1
3714 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
3715 '
3716     Mvs PTorqueCheck,-60                       '������mov ����ύX
3717     Dly 0.1
3718 '--------------------------------------------------------------
3719    'Ovrd 80
3720 '--------------------------------------------------------------
3721 '---------------------------------------
3722 '---------------------------------------
3723 '---------------------------------------�G���[���E����
3724    *LBL1
3725    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
3726    Mvs ,-100
3727    M_Out(12241)=0     '�h���C�o�[OFF CW
3728    Dly 0.1
3729    M_Out(12242)=0     '�h���C�o�[���� C1
3730    Dly 0.1
3731    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
3732    Dly 0.1
3733    M_Out(12245)=0     '�v���O�������� F1
3734 '---------------------------------------
3735 '---------------------------------------
3736 '-------------
3737    'Mov PInitPos19049
3738    Dly 0.1
3739 '
3740 '
3741     Exit Function
3742 FEnd
3743 '
3744 '��Main
3745 ''' <summary>
3746 ''' �g������p�̃��C��
3747 ''' </summary>
3748 ''' <remarks>
3749 ''' Date   : 2021/07/07 : M.Hayakawa
3750 ''' </remarks>'
3751 Function Main
3752     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3753     '
3754     If M_Svo=0 Then
3755         Servo On
3756     EndIf
3757     Wait M_Svo=1
3758 '�g���X�^�[�g���t�����v���p���XON
3759     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3760 '�p�g���C�g����
3761     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3762     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3763     '
3764     M_20# = 0                                   'KEY���͏�����
3765     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3766     MRet% = 0
3767 '�����ʒu�̊m�F�ƈړ�
3768 '
3769 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
3770     PActive = P_Curr                    '���݈ʒu���擾
3771     MRecoveryPass% = 0
3772     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3773         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3774             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3775                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3776             EndIf
3777         EndIf
3778     EndIf
3779     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3780         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3781             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3782                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3783             EndIf
3784         EndIf
3785     EndIf
3786     If MRecoveryPass% = 0 Then
3787        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3788     EndIf
3789 '
3790 '
3791 '    MRet% = fnRoboPosChk()
3792     If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ
3793         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3794         MKeyNumber% = fnKEY_WAIT()
3795         Select MKeyNumber%
3796             Case Is = MAbout%       '��~
3797                 M_20# = MAbout%
3798                 MLoopFlg% = -1
3799                 Break
3800             Case Is = MNext%        '����
3801                 'MLoopFlg = -1
3802                 Break
3803             Case Is = MContinue%    '�p��
3804                 M_20# = MContinue%
3805                 MLoopFlg% = -1
3806                 Break
3807             Default
3808                 Break
3809         End Select
3810     EndIf
3811     '
3812     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3813         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3814 '�g���N�`�F�b�N
3815         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3816             MRet% = fnTorqueCheck()
3817             Break
3818         Else
3819 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3820 '                MRtn = InspInit()               '�摜��������������
3821 '            EndIf
3822             '
3823            M_20# = MClear%                    '������
3824 '�g���J�n
3825             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3826 '                MRet% = fnAssyStart()
3827                 fnAssyStart()
3828             Else
3829                 M_20# = MPass%
3830             EndIf
3831 '�g���I�����t����
3832             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3833             Wait M_In(11572) = 1            '���t�擾����
3834             Dly 0.1
3835             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3836 '���t�^�[���j�b�g�ւ�OUT
3837             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3838             fnAutoScreenComment(89)         'AUTO��� �g����������
3839             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3840 'OK/NG�t���O�o��
3841             If M_20# <= 0 Then
3842                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3843             ElseIf M_20# = MPass% Then
3844                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3845             EndIf
3846 'PIAS�ɑg������������
3847             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3848                 If M_20# = MPass% Then
3849                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3850                 Else
3851                     'KEY���͂�NG�̏ꍇ
3852                     If M_20# = MNgProcess% Then
3853                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3854                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3855                         MRet% = fnPiasWrite(MNG%)
3856                        nAssyNgQty = nAssyNgQty + 1
3857                     EndIf
3858                     '
3859                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3860                     If M_20# = MAssyOK% Then
3861                             '-----------------------
3862                             'D732 -> D2600 �R�s�[�v��
3863                             M_Out(12566) = 1
3864 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3865                             M_Out(12566) = 0
3866                             '
3867                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3868                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3869                             '��ԍ��ƍ�(PP�͖��g�p�j
3870 '                            MRet% = fnPCBNumberCheck()
3871                         Else
3872                             MRet% = 1
3873                         EndIf
3874                         '
3875                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3876                             If M_20# <> MAbout% Then
3877                                 '�H������OK��������
3878                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3879                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3880                                 MRet% = fnPiasWrite(MOK%)
3881                                 nAssyOkQty = 0
3882                                 nAssyOkQty = nAssyOkQty + 1
3883                             Else
3884                                 nAssyOkQty = nAssyOkQty + 1
3885                             EndIf
3886                         EndIf
3887                     EndIf
3888 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3889 '                    MRet% = fnPiasWrite(MOK%)
3890                 EndIf
3891             Else
3892                 nAssyOkQty = nAssyOkQty + 1
3893             EndIf
3894             '
3895             '�g���I�����t��������
3896             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3897             '�������A�g��OK���A�g��NG��������
3898 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3899             '
3900 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3901 '                '�摜�����I������
3902 '                MRtn = InspQuit()
3903 '            EndIf
3904         EndIf
3905         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3906     EndIf
3907 '�p�g���C�g����
3908     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3909     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3910 'GOT�\��
3911     fnAutoScreenComment(93)  'AUTO��� �H������
3912 FEnd
3913 End
3914 '
3915 '���܂��Ȃ��R�����g
3916 '��΍폜�����
3917 '
3918 '
3919 '
3920 '
JActive=(-10.290,46.470,65.420,-0.100,67.610,-9.980,0.000,0.000)
Jmove=(-10.290,-44.940,112.380,0.000,76.270,0.000,0.000,0.000)
JTaihi=(0.000,-44.940,112.380,0.000,76.270,0.000,0.000,0.000)
PActive=(601.260,-152.240,450.960,180.000,0.000,90.000,0.000,0.000)(7,0)
PEscapePosition=(247.610,-0.380,580.000,-180.000,0.000,-179.990)(7,0)
PEscapePosition_2=(-131.770,177.640,579.990,-180.000,0.000,-53.320)(7,0)
PEscapePosition_3=(-178.950,226.160,579.830,-180.000,-0.020,-53.330)(7,0)
PEscapePosition_4=(-263.930,0.030,580.000,-180.000,0.000,0.100)(7,0)
PInitialPosition=(300.000,0.000,440.000,-180.000,0.000,-180.000)(7,0)
Pmove=(546.020,-99.260,400.000,-179.820,0.470,179.730,0.000,0.000)(7,0)
PProductOnJigGet=(-243.200,-0.160,427.020,30.910,88.850,-149.690)(6,1)
PProductOnJigGet_1=(-243.200,-0.160,460.000,30.910,88.850,-149.690)(6,1)
PProductOnJigGet_2=(-190.000,-0.160,559.980,37.460,88.990,-143.250)(6,1)
PProductOnJigGet_3=(-135.390,133.310,559.950,38.070,89.000,172.380)(6,0)
PProductOnJigGet_4=(-164.650,0.000,671.530,-166.660,90.000,13.340)(7,0)
PProductOnJigGet_5=(-224.300,0.010,604.770,180.000,0.000,0.000)(7,0)
PProductOnJigGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnJigSet=(-243.200,-0.160,427.020,30.910,88.850,-149.690)(6,1)
PProductOnJigSet_1=(-243.200,-0.160,460.000,30.910,88.850,-149.690)(6,1)
PProductOnJigSet_2=(-190.000,-0.160,559.990,37.460,88.980,-143.250)(6,1)
PProductOnJigSet_3=(-137.460,131.730,570.000,-172.800,89.910,-36.530)(6,0)
PProductOnJigSet_4=(139.330,129.750,570.000,-172.930,89.910,-129.920)(6,0)
PProductOnPltGet=(546.010,-99.240,250.980,-179.820,0.470,179.730)(7,0)
PProductOnPltGet_1=(546.010,-99.240,290.000,-179.820,0.470,179.730)(7,0)
PProductOnPltGet_2=(546.010,-99.240,400.000,-179.820,0.470,179.730)(7,0)
PProductOnPltSet=(544.890,-99.240,250.480,-179.820,0.470,179.730)(7,0)
PProductOnPltSet_1=(544.890,-99.240,290.000,-179.820,0.470,179.730)(7,0)
PProductOnPltSet_2=(544.890,-99.240,400.000,-179.820,0.470,179.730)(7,0)
PProductOnPltSet_3=(139.330,129.750,570.000,-172.920,89.910,-129.910)(6,0)
PScrewHeatSink1=(-376.000,84.190,567.280,-180.000,0.000,0.000)(7,0)
PScrewHeatSink1_0=(-376.000,84.190,575.780,-180.000,0.000,0.000)(7,0)
PScrewHeatSink1_1=(-376.000,84.190,620.000,-180.000,0.000,0.000)(7,0)
PScrewHeatSink2=(-375.800,110.800,567.190,-180.000,0.000,0.000)(7,0)
PScrewHeatSink2_0=(-375.800,110.800,575.990,180.000,0.000,0.000)(7,0)
PScrewHeatSink2_1=(-375.800,110.800,620.000,-179.990,0.050,0.000)(7,0)
PScrewPlateL=(-284.780,112.540,536.900,-180.000,0.000,-90.000)(7,0)
PScrewPlateL1=(-314.190,29.000,536.160,-180.000,0.000,-90.000)(7,0)
PScrewPlateL1_0=(-314.190,29.000,542.000,180.000,0.000,-90.000)(7,0)
PScrewPlateL1_1=(-314.190,29.000,570.000,180.000,0.000,-90.000)(7,0)
PScrewPlateL2=(-314.330,103.970,535.830,180.000,0.000,-90.000)(7,0)
PScrewPlateL2_0=(-314.330,103.970,540.950,180.000,0.000,-90.000)(7,0)
PScrewPlateL2_1=(-314.330,103.970,570.000,180.000,0.000,-90.000)(7,0)
PScrewPlateL_0=(-284.780,112.540,543.530,-180.000,0.000,-90.000)(7,0)
PScrewPlateL_1=(-284.780,112.540,570.000,-180.000,0.000,-90.000)(7,0)
PScrewPlateR=(-286.120,-114.330,536.390,-180.000,0.000,-90.000)(7,1)
PScrewPlateR1=(-314.100,-26.710,535.810,-180.000,0.000,-89.940)(7,1)
PScrewPlateR1_0=(-314.100,-26.710,541.970,180.000,0.000,-89.940)(7,1)
PScrewPlateR1_1=(-314.100,-26.710,570.000,180.000,0.000,-89.940)(7,1)
PScrewPlateR2=(-315.100,-101.790,536.270,-179.990,0.000,-89.940)(7,1)
PScrewPlateR2_0=(-315.100,-101.790,541.210,-179.990,0.000,-89.940)(7,1)
PScrewPlateR2_1=(-315.100,-101.790,570.000,-179.990,0.000,-89.940)(7,1)
PScrewPlateR_0=(-286.120,-114.330,543.600,-180.000,0.000,-90.000)(7,1)
PScrewPlateR_1=(-286.120,-114.330,570.000,-180.000,0.000,-90.000)(7,1)
PScrewSupplyHS=(-281.270,-239.640,400.970,-179.990,0.000,-58.700)(7,1)
PScrewSupplyHS_1=(-281.270,-239.640,422.840,-179.990,0.000,-58.700)(7,1)
PScrewSupplyHS_2=(-147.310,-147.280,610.000,-180.000,0.000,-45.010)(7,1)
PScrewSupplyHS_3=(-235.180,-0.020,609.980,180.000,0.000,0.000)(7,1)
PScrewSupplyHS_4=(-271.460,-216.690,422.800,-180.000,0.000,-90.000)(7,1)
PScrewSupplyHS_5=(-164.650,0.000,671.530,-166.660,90.000,13.340)(7,0)
PScrewSupplyPlate=(-134.450,198.940,450.150,-180.000,0.010,170.740)(7,0)
PScrewSupplyPlate_1=(-134.450,198.940,470.000,180.000,0.010,170.740)(7,0)
PScrewSupplyPlate_2=(-135.250,198.940,570.000,-180.000,0.010,170.740)(7,0)
PScrewSupplyPlate_3=(-132.530,160.720,570.000,-179.990,0.000,-140.430)(7,0)
PScrewSupplyPlate_4=(-208.310,0.230,610.000,-180.000,0.000,-90.000)(7,0)
PScrewSupplyPlate_5=(-113.970,113.990,696.720,-0.320,89.240,134.680)(7,0)
PScrewSupplyPlate_6=(-161.190,0.020,696.720,-0.320,89.240,179.670)(7,0)
PScrewSupplyPlate_7=(-50.580,239.350,544.690,-180.000,0.010,170.740)(7,0)
PScrewSupplyPlatel_2=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PTemp=(601.260,-152.240,450.960,180.000,0.000,90.000,0.000,0.000)(7,0)
PTicketRead=(601.260,-152.240,374.960,-180.000,0.000,90.000)(7,0)
PTicketRead_1=(601.260,-152.240,450.960,-180.000,0.000,90.000)(7,0)
PTorqueCheck=(148.860,-273.360,336.080,-180.000,-0.010,110.020)(7,0)
PTorqueCheck_1=(148.840,-273.370,360.500,-180.000,-0.010,110.020)(7,0)
PEscapePosi(1)=(-252.420,-263.310,570.000,179.960,-0.070,-43.810)(7,1)
PEscapePosi(2)=(-141.510,-152.870,570.000,179.960,-0.070,-43.810)(7,1)
PEscapePosi(3)=(-208.310,0.230,570.000,-180.000,0.000,-90.000)(7,0)
PEscapePosi(4)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(5)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(6)=(-208.310,0.010,610.000,180.000,0.000,0.000)(7,0)
PEscapePosi(7)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(8)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(9)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(10)=(0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PGetScrewPos(1)=(-134.450,198.940,470.000,180.000,0.010,170.740,0.000,0.000)(7,0)
PGetScrewPos(2)=(-135.250,198.940,570.000,-180.000,0.010,170.740,0.000,0.000)(7,0)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(-50.580,239.350,544.690,-180.000,0.010,170.740,0.000,0.000)(7,0)
PGetScrewPos(10)=(-134.450,198.940,450.150,-180.000,0.010,170.740,0.000,0.000)(7,0)
PInspPosition(1)=(601.260,-152.240,374.960,-180.000,0.000,90.000,0.000,0.000)(7,0)
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
PScrewPos(1)=(-315.100,-101.790,570.000,-179.990,0.000,-89.940,0.000,0.000)(7,1)
PScrewPos(2)=(-315.100,-101.790,541.210,-179.990,0.000,-89.940,0.000,0.000)(7,1)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(-315.100,-101.790,536.270,-179.990,0.000,-89.940,0.000,0.000)(7,1)
