1 ' ===================================
2 '
3 '  21050001 STEP5 Assy2プログラム
4 '
5 ' 作成者：自動化T
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 ' Ver 0.3 2021.12.22 画像検査関数ISInspection→ISInspectionSingle、画像検査追加 file:210542003
9 ' ===================================
10 '===== <Insight定数> =====
11 '===== <Insight変数定義> =====
12 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
13 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
14 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
15 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
16 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
17 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
18 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
19 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
20 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
21 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
22 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
23 '
24 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
25 'Output Signal
26 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
27 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
28 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
29 '
30 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
31 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
32 '
33 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
34 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
35 '作業用変数
36 Def Inte MInspErrNum                '検査実行エラー番号
37 Def Inte MInspNGStepNum             '検査実行NGStep番号
38 Def Inte MRtn                       'Function戻り値取得用
39 Def Inte MRtn2                      'Function戻り値取得用
40 Def Inte MRet3                      'Function戻り値取得用
41 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
42 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
43 Def Inte MOvrdA                     'ネジ締めOvrd 可変用
44 Def Float MSpdA                     'ネジ締めSpd　可変用
45 Def Pos PTemp                       'ネジ締め上空位置計算用
46 '===== <Insight変数設定> =====
47 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
48 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
49 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
50 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
51 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
52 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
53 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
54 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
55 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
56 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
57 'Output Signal
58 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
59 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
60 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
61 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
62 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
63 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
64 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
65 '===== <電ドラ変数定義> =====
66 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
67 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
68 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
69 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
70 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
71 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
72 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
73 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
74 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
75 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
76 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
77 '11584   'toRBトルクドライバ-COMP_ERR送信
78 Y60_Driver=12240 '電ドラ半時計回り CCW
79 Y61_Driver=12241 '電ドラ時計回り CW
80 Y62_Driver=12242 'バンクセッティング BANK C1
81 Y63_Driver=12243 'バンクセッティング BANK C2
82 Y64_Driver=12244 'バンクセッティング BANK C3
83 Y65_Driver=12245 'プログラムセッティング PRG SET F1
84 Y66_Driver=12246 'プログラムセッティング PRG SET F2
85 Y67_Driver=12247 'プログラムセッティング PRG SET F3
86 X34_ScrewReady1=11259 'ねじっこ1　Read
87 '===== <電ドラ定数> =====
88 Dim PScrewPos(10)       'ネジ締め用Function引数変数
89 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
90 Dim PEscapePosi(10)
91 MLoopCnt% = 0'
92 '===== <ロボット定数> =====
93 '===== <ロボット変数定義> =====
94 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
95 MCommentD1001 = 0
96 MCommentD1002 = 0
97 MCommentD1003 = 0
98 MScreenNo = 0
99 '
100 MCommentTSU = 0
101 MCommentTSD = 0
102 'ウィンド画面番号設定
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
115 MClear% = 0        'KEY_のクリア
116 MAbout% = 1        'KEY_停止
117 MNext% = 2         'KEY_次のステップへ移行
118 MContinue% = 3     'KEY_継続 再度同じ動作を行う
119 '
120 Def Inte MNgProcess
121 MNgProcess% = 5      'KEY_NG
122 '
123 MAssyOK% = 6       '組立完了
124 MPass% = 7         '工程パス
125 MPiasNG% = 8       'Pias確認時履歴NG
126 '
127 '初期化用KEY番号   '
128 MRobotInit1% = 11  '初期位置用
129 MRobotInit2% = 12  '初期位置用
130 MRobotInit3% = 13  '初期位置用
131 MRobotInit4% = 14  '初期位置用
132 '
133 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
134 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
135 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
136 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
137 '
138 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
139 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
140 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
141 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
142 '
143 MOK% = 1               '各判定用
144 MNG% = 0               '各判定用
145 MTIMEOUT% = -1         '各判定用
146 MJudge% = 0            '判定情報格納用
147 '
148 MRECIVETIME& = 0
149 MSETTIMEOUT10& = 10000&                '10秒設定
150 MSETTIMEOUT03& = 3000&                 '3秒設定
151 MSETTIMEOUT01& = 1000&                 '1秒設定
152 MSETTIMEOUT05& = 5000&                 '5秒設定
153 MSETTIMEOUT009& = 900&                 '0.9秒設定
154 MSETTIMEOUT008& = 800&                 '0.8秒設定
155 MSETTIMEOUT007& = 700&                 '0.7秒設定
156 MSETTIMEOUT006& = 600&                 '0.6秒設定
157 MSETTIMEOUT005& = 500&                 '0.5秒設定
158 MSETTIMEOUT004& = 400&                 '0.4秒設定
159 MSETTIMEOUT003& = 300&                 '0.3秒設定
160 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
161 MIN_PIAS_ComOK% = 11552                'PC通信OK
162 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
163 MIN_PIAS_ComNG% = 11553                'PC通信NG
164 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
165 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
166 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
167 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
168 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
169 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
170 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
171 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
172 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
173 '
174 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
175 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
176 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
177 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
178 '
179 MOUT_PiasAssyResultOK% = 12549    '組立OK
180 MOUT_PiasAssyResultNG% = 12550    '組立NG
181 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
182 '
183 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
184 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
185 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
186 '
187 MIN_Insight_Use% = 11369               '画像確認ON
188 MIN_TorqueCheck% = 11348               'トルクチェック
189 '
190 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
191 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
192 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
193 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
194 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
195 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
196 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
197 '
198 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
199 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
200 '
201 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
202 '
203 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
204 '
205 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
206 MopeNo% = 0
207 MOvrdA% = 10
208 MRtn% = 0
209 MRet = 0
210 MRet3% = 0
211 '
212 Def Inte MInputQty          '投入数 演算変数
213 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
214 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
215 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
216 Def Inte nAssyOkQty         '未使用
217 Def Inte MScrewNo
218 Def Inte MReTry
219 '===== <IO変数定義> =====
220 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
221 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
222 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
223 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
224 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
225 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
226 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
227 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
228 '
229 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
230 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
231 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
232 '
233 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
234 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
235 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
236 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
237 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
238 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
239 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
240 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
241 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
242 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
243 '
244 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
245 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
246 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
247 '
248 Def Inte MOUT_LED1          ' 画像処理用LED照明
249 '
250 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
251 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
252 '
253 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
254 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
255 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
256 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
257 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
258 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
259 '
260 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
261 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
262 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
263 '
264 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
265 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
266 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
267 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
268 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
269 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
270 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
271 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ '数値12250から12248へ変更(8/5中村)
272 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
273 MOUT_VB1%   =  12250    ' アーム先端　ネジ吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
274 '
275 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
276 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
277 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
278 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
279 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
280 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
281 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
282 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
283 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
284 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
285 '
286 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
287 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
288 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
289 '
290 MOUT_LED1%  =  12239    ' 画像処理用LED照明
291 '
292 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
293 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
294 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
295 '
296 '共通
297 Def Inte MTEST_KEY                      'デバックテスト用
298 Def Inte MOn                            '出力=1
299 Def Inte MOff                           '出力=0
300 '
301 'ねじ締め装置_出力アドレス
302 Def Inte MOUT_ScwT_ComChk               '通信確認
303 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
304 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
305 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
306 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
307 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
308 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
309 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
310 'ねじ締め装置_入力アドレス
311 Def Inte MIN_ScwT_comOK                 '通信確認返信
312 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
313 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
314 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
315 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
316 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
317 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
318 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
319 '
320 Def Inte MRetryLimit                    ' リトライ回数
321 Def Inte MRetryCount                    ' リトライカウント
322 '
323 Dim MScwT_Case1%(2)               '条件1停止変数
324 Dim MScwT_Case2%(2)               '条件2停止変数
325 Dim MScwT_Case3%(2)               '条件3停止変数
326 Dim MScwT_Case4%(2)               '条件4停止変数
327 Dim MScwT_Case5%(2)               '条件5停止変数
328 '
329 Def Pos PActive                     '直交座標系 位置変数 現在位置
330 Def Pos Pmove                       '直交座標系 位置変数 移動先
331 Def Inte MRecoveryPass              '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行'
332 '共通
333 MTEST_KEY% = 11359                       'デバッグ用テストKEY
334 MOn% = 1                                 '出力 = 1
335 MOff% = 0                                '出力 = 0
336 '
337 'ねじ締め機_アドレス設定
338 MOUT_ScwT_ComChk% = 12816               '通信確認送信
339 MOUT_ScwT_ST% = 12849                   'ねじ締め開始を送信
340 MOUT_ScwT_ReSTOK% = 12850               '再開始受信を送信
341 MOUT_ScwT_FinOK% = 12852                'ねじ締め完了受信を送信
342 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
343 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
344 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
345 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
346 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
347 '
348 MIN_ScwT_comOK% = 11824                 'ねじ締め装置から返信
349 MIN_ScwT_STRec% = 11857                 'ねじ締め開始を受信
350 MIN_ScwT_ReST% = 11858                  '再開始を受信
351 MIN_ScwT_Fin% = 11860                   'ねじ締め完了を受信
352 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
353 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
354 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
355 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
356 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
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
371 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
372 Function M% fnAssyStart
373     M_20# = MClear%                       '初期化
374 '組み立て開始
375     Ovrd 100
376 '初期位置を設定
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
389     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
390     If MRtn = 1 Then
391         M_Out(12262) = 1            '位置決め出ON
392         Mov PTicketRead
393         Break
394     Else
395         Mov PInitialPosition
396         M_Out(12262) = 1            '位置決め出ON
397         Mov PTicketRead_1           'チケットID読み取り回避点
398         Mvs PTicketRead             'ID読み位置
399         Break
400     EndIf
401 '
402     MRtn = 1                        'MRtn初期化
403     *RE_TICKET_READ
404 '    MRtn = fnPiasCheck()               'ID読み取り
405 '    PInspPosition(1) = PTicketRead  'IDチケット読取位置
406 '    MInspGroup%(1) = 1              '検査G番号
407 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
408     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
409         MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
410         '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
411         '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
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
425     'ハンドの状態をイニシャルに戻す
426     MRtn =frInCheck(11264,0,MSETTIMEOUT05&) 'PCB検出(あるとエラー)
427     If MRtn = 1 Then GoTo *CompCheck_1
428     fErrorProcess(11,230,281,0)     '0→230に変更6/8中村
429     If M_20# = MNext% Then M_20# = MClear%
430     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
431     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
432     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
433     *CompCheck_1
434     '
435     If M_In(11266) = 1 Then
436         M_Out(12256) = 0        'PCBチャック開OFF
437         M_Out(12257) = 1        'PCBチャック閉ON
438         Break
439     EndIf
440     If M_In(11268) = 1 Then
441         M_Out(12258) = 0        'PCBシリンダー出OFF
442         M_Out(12259) = 1        'PCBシリンダー戻ON
443         Break
444     EndIf
445     If M_In(11270) = 1 Then
446         M_Out(12260) = 0        'BtoBシリンダー出OFF
447         M_Out(12261) = 1        'BtoBシリンダー戻ON
448         Break
449     EndIf
450     '
451     MRtn =frInCheck(11265,1,MSETTIMEOUT05&) 'PCBチャック閉検出
452     If MRtn = 1 Then GoTo *CompCheck_2
453     fErrorProcess(11,240,281,0)
454     If M_20# = MNext% Then M_20# = MClear%
455     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
456     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
457     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
458     *CompCheck_2
459     '
460     MRtn =frInCheck(11267,1,MSETTIMEOUT05&) 'PCBシリンダー戻検出
461      If MRtn = 1 Then GoTo *CompCheck_3
462     fErrorProcess(11,239,281,0)
463     If M_20# = MNext% Then M_20# = MClear%
464     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
465     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
466     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
467     *CompCheck_3
468     '
469     MRtn =frInCheck(11269,1,MSETTIMEOUT05&) 'BtoBシリンダー戻検出
470     If MRtn = 1 Then GoTo *CompCheck_4
471     fErrorProcess(11,243,281,0)
472     If M_20# = MNext% Then M_20# = MClear%
473     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
474     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
475     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
476     *CompCheck_4
477     '
478 '---------------------------------------------------------------
479     '圧力設定(低圧)07/30中村
480     M_Out(12266) = 1
481     M_Out(12267) = 0
482 '---------------------------------------------------------------
483 '
484 '
485     '製品位置決め
486     *RE_POSITIONING        '位置決めリトライ用
487     M_Out(12262)=1 Dly 0.3      '位置決めパルス信号
488     'Wait M_In(11273)=1          '位置決め出端検出(修正につきコメントアウト(8/26中村))
489     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)    '位置決め出端検出(8/26中村)
490     If MRtn = 1 Then GoTo *CompPosition_1
491     fErrorProcess(11,231,282,0)
492     If M_20# = MNext% Then M_20# = MClear%
493     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
494     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
495     If M_20# = MContinue% Then GoTo *RE_POSITIONING
496     *CompPosition_1
497     '
498     Dly 0.5
499     M_Out(12264)=1 Dly 0.3      'プッシュパルス信号
500     'Wait M_In(11275)=1          'プッシュ出端検出(修正につきコメントアウト(8/26中村))
501     MRtn = frInCheck(11275,1,MSETTIMEOUT05&)    'プッシュ位置出端検出(8/26中村)
502     If MRtn = 1 Then GoTo *CompPosition_2
503     fErrorProcess(11,232,282,0)
504     If M_20# = MNext% Then M_20# = MClear%
505     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
506     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
507     If M_20# = MContinue% Then GoTo *RE_POSITIONING
508     *CompPosition_2
509 '
510     'SOC基板を取る
511     *RE_GET_SOC
512     '
513     Mov PSocGet_2               '基板ピックアップ回避点  Y:変更 107.160→106.160
514     M_Out(12256)=0              '基板チャック開OFF
515     M_Out(12257)=1              '基板チャック閉ON
516     '
517     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'チャック閉センサーON
518     If MRtn = 1 Then GoTo *CompGetSOC_1
519     fErrorProcess(11,240,284,0)
520     If M_20# = MNext% Then M_20# = MClear%
521     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
522     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
523     If M_20# = MContinue% Then GoTo *RE_GET_SOC
524     *CompGetSOC_1
525     '
526     M_Out(12259)=0              'PCBシリンダー戻OFF
527     M_Out(12258)=1              'PCBシリンダー出ON
528     Dly 0.2
529     '
530 '    Wait M_In(11268)=1          'PCBシリンダー出端センサーON
531     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)
532     If MRtn = 1 Then GoTo *CompGetSOC_2
533     fErrorProcess(11,238,284,0)
534     If M_20# = MNext% Then M_20# = MClear%
535     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
536     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
537     If M_20# = MContinue% Then GoTo *RE_GET_SOC
538     *CompGetSOC_2
539     '
540     Mov PSocGet_1               '基板上空 Y:変更 107.160→106.160
541     Ovrd 40
542     Mvs PSocGet                 '基板ピックアップ位置  Y:変更 107.170→106.170
543     Dly 0.3
544     Ovrd 5                      '2021-12-19追加 AJ
545     '
546     '
547 '    Wait M_In(11264)=1          'PCB検出センサーON
548     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
549     If MRtn = 1 Then GoTo *CompGetSOC_3
550     fErrorProcess(11,299,291,0)     '0,284→299.291に変更6/8中村
551     If M_20# = MNext% Then M_20# = MClear%
552     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
553     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
554     If M_20# = MContinue% Then GoTo *RE_GET_SOC
555     *CompGetSOC_3
556     '
557     M_Out(12257)=0              '基板チャック閉OFF
558     M_Out(12256)=1              '基板チャック開ON
559     Dly 0.2
560     '
561 '    Wait M_In(11266)=1          'チャック開センサーON
562     MRtn = frInCheck(11266 , 1 , MSETTIMEOUT05&)
563     Mvs PSocGet_1               '基板上空  Y:変更 107.160→106.160
564     If MRtn = 1 Then GoTo *CompGetSOC_4
565     Mov PSocGet_2               ' Y:変更 107.160→106.160
566     fErrorProcess(11,241,284,0)
567     If M_20# = MNext% Then M_20# = MClear%
568     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
569     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
570     If M_20# = MContinue% Then GoTo *RE_GET_SOC
571     *CompGetSOC_4
572     '
573     'Wait M_In(11264)=1          'PCB検出センサーON
574     '
575     '下記、PCB判定とエラー処理追加 2021-12-19 AJ
576     MRtn = frInCheck(11264 , 1 , MSETTIMEOUT05&)
577     If MRtn = 1 Then GoTo *CompGetSOC_41
578     fErrorProcess(11,299,291,0)     '0,284→299.291に変更6/8中村
579     If M_20# = MNext% Then M_20# = MClear%
580     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
581     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
582     If M_20# = MContinue% Then GoTo *RE_GET_SOC
583     *CompGetSOC_41
584     '
585     '
586     'Ovrd 100                   '位置変更2021-12-19追加AJ
587     Ovrd 15                     '追加1/17中村
588     Mov PSocGet_2               '基板ピックアップ回避点
589     Ovrd 100                    '2021-12-19追加AJ
590     '
591     'SOC基板を製品上に置く
592     Mov PSocSet_2               '基板置き回避点
593     Mov PSocSet_1               '製品上空
594     Dly 0.1
595     Ovrd 40
596     Mvs PSocSet                 '基板置き位置（空中で離す）
597     M_Out(12256)=0              '基板チャック開OFF
598     M_Out(12257)=1              '基板チャック閉ON
599 '
600     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
601     Mvs PSocSet_1               '製品上空
602     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
603 '
604     'Wait M_In(11265)=1          'チャック閉センサーON
605     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)
606     If MRtn = 1 Then GoTo *CompGetSOC_5
607     fErrorProcess(11,240,284,0)
608     If M_20# = MNext% Then M_20# = MClear%
609     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
610     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
611     If M_20# = MContinue% Then GoTo *RE_GET_SOC
612     *CompGetSOC_5
613     '
614 '    Wait M_In(11268)=1          'PCBシリンダー出端センサーON・・・もしOFFだったら基板挿入を失敗している
615     MRtn = frInCheck(11268 , 1 , MSETTIMEOUT05&)
616     If MRtn = 1 Then GoTo *CompGetSOC_6
617     fErrorProcess(11,238,284,0)
618     If M_20# = MNext% Then M_20# = MClear%
619     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
620     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
621     If M_20# = MContinue% Then GoTo *RE_GET_SOC
622     *CompGetSOC_6
623     '
624     'Wait M_In(11264)=0          'PCB検出センサーOFF
625     M_Out(12258)=0              'PCBシリンダー出OFF
626     M_Out(12259)=1              'PCBシリンダー戻ON
627     '
628 '    Wait M_In(11267)=1          'PCBシリンダー戻端センサーON
629     MRtn = frInCheck(11267 , 1 , MSETTIMEOUT05&)
630     If MRtn = 1 Then GoTo *CompGetSOC_7
631     fErrorProcess(11,239,284,0)
632     If M_20# = MNext% Then M_20# = MClear%
633     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
634     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
635     If M_20# = MContinue% Then GoTo *RE_GET_SOC
636     *CompGetSOC_7
637     Ovrd 100
638     Mov PSocSet_2               '基板置き回避点
639 '【SOC基板ID読み込み】
640     *RE_SOC_CHECK1
641     PInspPosition(1) = PSocPcbRead  'SOC基板ID読取位置
642     MInspGroup%(1) = 2              '検査G番号
643     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
644 '
645     If MRtn = 1 Then GoTo *CompSocCheck1
646     fErrorProcess(11,97,25,0)
647     If M_20# = MNext% Then M_20# = MClear%
648     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
649     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
650     If M_20# = MContinue% Then GoTo *RE_SOC_CHECK1
651     *CompSocCheck1
652 '【基板IDコピー】
653     *RE_PCB_RECORD
654     M_Out(12571) = 1    ' 領域1 基板番号コピー (D2600-) On
655     Dly 0.1
656     M_Out(12566) = 1    ' toPLC_基板番号コピー要求 On
657 '
658     MRtn = frInCheck(11581,1,MSETTIMEOUT05&)    ' toRBT_基板番号コピー完了 On
659     If MRtn = 1 Then
660         M_Out(12571) = 0  ' 領域1 基板番号コピー (D2600-) Off
661         Dly 0.1
662         M_Out(12566) = 0  ' toPLC_基板番号コピー要求 Off
663 '        GoTo *RE_PCB_COMPAIRE   ' 基板番号照合にスキップ
664     Else
665         fErrorProcess(11,39,25,0)
666         If M_20# = MNext% Then M_20# = MClear%
667         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
668         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
669         If M_20# = MContinue% Then GoTo *RE_PCB_RECORD
670     EndIf
671 '【基板ID照合（紐付け）】
672     MRetryCount% = 0
673     While (MRetryCount% <= MRetryLimit%)
674         *RE_PCB_COMPAIRE
675         M_Out(12557)= 1 ' 基板番号照合ビットON
676         MRtn = frInCheck(11566,1,MSETTIMEOUT05&)    ' toRBT_基板番号照合OK(M420) On
677         If MRtn = 1 Then
678             M_Out(12557)= 0     ' 基板番号照合ビットOff
679             ' リトライ回数設定でループを抜ける
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
690                     ' リトライ回数設定でループを抜ける
691                     MRetryCount% = 99
692                 EndIf
693                 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
694                 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
695                 If M_20# = MContinue% Then
696                     MRetryCount% = 0
697                 EndIf
698             Else
699                 ' リトライ回数インクリメント
700                 MRetryCount% = MRetryCount% + 1
701                 Dly 0.5 ' 他の工程とタイミングをずらす為のディレイ
702             EndIf
703         EndIf
704     WEnd
705 '
706 '【Soc基板画像チェック】
707 '    *RE_SOC_CHECK2
708 '    PInspPosition(1) = PSocCheck    'Soc基板画像チェック位置
709 '    MInspGroup%(1) = 3              '検査G番号
710 '    MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
711 '    If MRtn = 1 Then GoTo *CompSocCheck2
712 '    fErrorProcess(11,43,46,0)
713 '    If M_20# = MNext% Then M_20# = MClear%
714 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
715 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
716 '    If M_20# = MContinue% Then GoTo *RE_SOC_CHECK2
717 '    *CompSocCheck2
718     '基板置き位置画像検査（不要？）
719     '
720     'SOC基板BtoBプレス
721     'Mov PSocPress_2             'BtoBプレス回避点
722     *RE_BtoBPRESS   'BtoBシリンダーリトライ
723     Mov PSocPress_1             'プレス上空
724     M_Out(12261)=0              'プレスシリンダー戻OFF
725     M_Out(12260)=1              'プレスシリンダー出ON
726     Dly 0.2
727     '
728     'Wait M_In(11270)=1          'プレスシリンダー出端センサーON(修正につきコメントアウト(8/27中村))
729     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)    'プレスシリンダー出端センサーON(8/27中村)
730     If MRtn = 1 Then GoTo *CompPress_1
731     fErrorProcess(11,242,284,0)
732     If M_20# = MNext% Then M_20# = MClear%
733     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
734     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
735     If M_20# = MContinue% Then GoTo *RE_BtoBPRESS
736     *CompPress_1
737 '
738 '--------------------------------------------
739     '圧力検出(低圧力)
740     '22/07/29追加 中村
741 '--------------------------------------------
742 *RE_Pa_OUT
743     If M_20# = MContinue% Then
744     M_Out(12266) = 1
745     M_Out(12267) = 0
746     Dly 0.5
747     M_20# = MClear%
748     EndIf
749     MRtn = frInCheck(11277,1,MSETTIMEOUT05&)     'MDV用圧力検出(22/07/29中村)
750     MRtn2 = frInCheck(11278,0,MSETTIMEOUT05&)    'KA用圧力検出(ONで上がりすぎ)(22/07/29中村)
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
764     Mvs PSocPress               'プレスエンド端まで移動
765     Dly 0.5
766     'Wait M_In(11270)=1          'プレスシリンダー出端センサーON…もしOFFだったらコネクタカバー有
767     MRtn = frInCheck(11270,0,MSETTIMEOUT05&)    'プレスシリンダー出端センサーON(8/27中村)
768     If MRtn = 1 Then GoTo *CompPress_2
769     Mvs PSocPress_1              'プレス上空
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
780     M_Out(12260)=0              'プレスシリンダー出OFF
781     M_Out(12261)=1              'プレスシリンダー戻ON
782 '    Wait M_In(11269)=1          'プレスシリンダー戻端センサーON
783     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)    'プレスシリンダー戻端センサーON(8/27中村)
784     If MRtn = 1 Then GoTo *CompBtoBRest
785     fErrorProcess(11,243,284,0)
786     If M_20# = MNext% Then M_20# = MClear%
787     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
788     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
789     If M_20# = MContinue% Then GoTo *RE_BtoB_REST
790     *CompBtoBRest
791     '
792     Ovrd 100
793     Mov PSocPress_1             'プレス上空
794     M_Out(12266) = 0
795     M_Out(12267) = 0
796     'Mov PSocPress_2            'BtoBプレス回避点
797     '
798 'Soc基板ネジ締め
799     PGetScrewPos(1) = PScrewSupply_1        ' ねじピックアップ上空を代入
800     PGetScrewPos(2) = PScrewSupply_2        ' ねじ供給機回避点を代入
801     PGetScrewPos(9) = PScrewSupply_9        ' ネジ供給機上空ネジ捨て位置(10/6 M.H追加)
802     PGetScrewPos(10) = PScrewSupply         ' ねじピックアップ上空を代入
803     '
804     'Soc基板用ネジ供給機へネジを取りに行く
805     *RE_SCREW_GET_1
806     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
807     If MRtn = 1 Then GoTo *CompScrewGet_1
808     If M_20# = MNext% Then M_20# = MClear%
809     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
810     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
811     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
812     *CompScrewGet_1
813     '
814     PScrewPos(1) = PScrewSoc1_1             ' ねじピックアップ上空を代入
815     PScrewPos(2) = PScrewSoc1_0             ' ネジ1締め開始位置を代入(10/8 M.H)
816     PScrewPos(10) = PScrewSoc1              ' ねじ供給機回避点を代入
817     '?@番ネジ締め
818     M_Out16(12672) = 1                      'ネジ締め位置番号送信
819     MRtn = ScrewTight(PScrewPos,1,10.0)
820     M_Out16(12672) = 0                      'ネジ締め位置番号クリア
821     If MRtn = 1 Then GoTo *CompScrew1
822     Mov PInitialPosition
823     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
824     MScrewErrorCord% = MScrewErrorCord% + 1
825     fErrorProcess(11,MScrewErrorCord%,52,0)
826 '    fErrorProcess(11,53,52,0)
827     If M_20# = MNext% Then M_20# = MClear%
828     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
829     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
830     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
831     *CompScrew1
832     '
833     'Soc基板用ネジ供給機へネジを取りに行く
834     *RE_SCREW_GET_2
835     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
836     If MRtn = 1 Then GoTo *CompScrewGet_2
837     If M_20# = MNext% Then M_20# = MClear%
838     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
839     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
840     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
841     *CompScrewGet_2
842     '
843     PScrewPos(1) = PScrewSoc2_1             ' ねじピックアップ上空を代入
844     PScrewPos(2) = PScrewSoc2_0             ' ネジ2締め開始位置を代入(10/8 M.H)
845     PScrewPos(10) = PScrewSoc2              ' ねじ供給機回避点を代入
846     '?A番ネジ締め
847     M_Out16(12672) = 2                      'ネジ締め位置番号送信
848     MRtn = ScrewTight(PScrewPos,1,10.0)
849     M_Out16(12672) = 0                      'ネジ締め位置番号クリア
850     If MRtn = 1 Then GoTo *CompScrew2
851     Mov PInitialPosition
852     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
853     MScrewErrorCord% = MScrewErrorCord% + 2
854     fErrorProcess(11,MScrewErrorCord%,52,0)
855 '    fErrorProcess(11,54,52,0)
856     If M_20# = MNext% Then M_20# = MClear%
857     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
858     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
859     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
860     *CompScrew2
861     '
862     'Soc基板用ネジ供給機へネジを取りに行く
863     *RE_SCREW_GET_3
864     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
865     If MRtn = 1 Then GoTo *CompScrewGet_3
866     If M_20# = MNext% Then M_20# = MClear%
867     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
868     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
869     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
870     *CompScrewGet_3
871     '
872     PScrewPos(1) = PScrewSoc3_1             ' ねじピックアップ上空を代入
873     PScrewPos(2) = PScrewSoc3_0             ' ネジ3締め開始位置を代入(10/8 M.H)
874     PScrewPos(10) = PScrewSoc3              ' ねじ供給機回避点を代入
875     '?B番ネジ締め
876     M_Out16(12672) = 3                      'ネジ締め位置番号送信
877     MRtn = ScrewTight(PScrewPos,1,10.0)
878     M_Out16(12672) = 0                      'ネジ締め位置番号クリア
879     If MRtn = 1 Then GoTo *CompScrew3
880     Mov PInitialPosition
881     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
882     MScrewErrorCord% = MScrewErrorCord% + 3
883     fErrorProcess(11,MScrewErrorCord%,52,0)
884 '    fErrorProcess(11,55,52,0)
885     If M_20# = MNext% Then M_20# = MClear%
886     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
887     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
888     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
889     *CompScrew3
890     '
891     'Soc基板用ネジ供給機へネジを取りに行く
892     *RE_SCREW_GET_4
893     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
894     If MRtn = 1 Then GoTo *CompScrewGet_4
895     If M_20# = MNext% Then M_20# = MClear%
896     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
897     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
898     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
899     *CompScrewGet_4
900     '
901     PScrewPos(1) = PScrewSoc4_1             ' ねじピックアップ上空を代入
902     PScrewPos(2) = PScrewSoc4_0             ' ネジ4締め開始位置を代入(10/8 M.H)
903     PScrewPos(10) = PScrewSoc4              ' ねじ供給機回避点を代入
904     '?C番ネジ締め
905     M_Out16(12672) = 4                      'ネジ締め位置番号送信
906     MRtn = ScrewTight(PScrewPos,1,10.0)
907     M_Out16(12672) = 0                      'ネジ締め位置番号クリア
908     If MRtn = 1 Then GoTo *CompScrew4
909     Mov PInitialPosition
910     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
911     MScrewErrorCord% = MScrewErrorCord% + 4
912     fErrorProcess(11,MScrewErrorCord%,52,0)
913 '    fErrorProcess(11,56,52,0)
914     If M_20# = MNext% Then M_20# = MClear%
915     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
916     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
917     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
918     *CompScrew4
919     '
920     'Soc基板用ネジ供給機へネジを取りに行く
921     *RE_SCREW_GET_5
922     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
923     If MRtn = 1 Then GoTo *CompScrewGet_5
924     If M_20# = MNext% Then M_20# = MClear%
925     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
926     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
927     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
928     *CompScrewGet_5
929     '
930     PScrewPos(1) = PScrewSoc5_1             ' ねじピックアップ上空を代入
931     PScrewPos(2) = PScrewSoc5_0             ' ネジ5締め開始位置を代入(10/8 M.H)
932     PScrewPos(10) = PScrewSoc5              ' ねじ供給機回避点を代入
933     '?D番ネジ締め
934     M_Out16(12672) = 5                      'ネジ締め位置番号送信
935     MRtn = ScrewTight(PScrewPos,1,10.0)
936     M_Out16(12672) = 0                      'ネジ締め位置番号クリア
937     If MRtn = 1 Then GoTo *CompScrew5
938     Mov PInitialPosition
939     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
940     MScrewErrorCord% = MScrewErrorCord% + 5
941     fErrorProcess(11,MScrewErrorCord%,52,0)
942 '    fErrorProcess(11,57,52,0)
943     If M_20# = MNext% Then M_20# = MClear%
944     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
945     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
946     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
947     *CompScrew5
948     '
949     'Soc基板用ネジ供給機へネジを取りに行く
950     *RE_SCREW_GET_6
951     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
952     If MRtn = 1 Then GoTo *CompScrewGet_6
953     If M_20# = MNext% Then M_20# = MClear%
954     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
955     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
956     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
957     *CompScrewGet_6
958     '
959     PScrewPos(1) = PScrewSoc6_1             ' ねじピックアップ上空を代入
960     PScrewPos(2) = PScrewSoc6_0             ' ネジ6締め開始位置を代入(10/8 M.H)
961     PScrewPos(10) = PScrewSoc6              ' ねじ供給機回避点を代入
962     '?E番ネジ締め
963     M_Out16(12672) = 6                      'ネジ締め位置番号送信
964     MRtn = ScrewTight(PScrewPos,1,10.0)
965     M_Out16(12672) = 0                      'ネジ締め位置番号クリア
966     If MRtn = 1 Then GoTo *CompScrew6
967     Mov PInitialPosition
968     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
969     MScrewErrorCord% = MScrewErrorCord% + 6
970     fErrorProcess(11,MScrewErrorCord%,52,0)
971 '    fErrorProcess(11,58,52,0)
972     If M_20# = MNext% Then M_20# = MClear%
973     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
974     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
975     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
976     *CompScrew6
977 '
978     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
979     Mov PTicketRead_1   'チケット読み取り位置上空
980     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
981     InitialState()  ' 初期状態にする*AssyEnd
982     M_20# = MAssyOK%              ' 正常終了処理
983     GoTo *fnAssyStart_FEndPosi
984 '
985 *ASSY_ERROR_END
986     fnInitialZone()   ' 初期位置に移動
987     InitialState()  ' 初期状態にする*AssyEnd
988 *AssyEnd
989 *fnAssyStart_FEndPosi
990     Exit Function
991 FEnd
992 '
993 '■fnPiasCheck
994 ''' <summary>
995 ''' PIASチケット読込み
996 ''' </summary>
997 ''' <returns>   0 : NG
998 '''             1 : OK(読込み完了)
999 ''' </returns>
1000 ''' <remarks>
1001 ''' Date   : 2021/07/07 : M.Hayakawa
1002 ''' </remarks>'
1003 Function M% fnPiasCheck
1004     fnPiasCheck = 0
1005     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1006     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1007 '
1008 *RETRY_PIAS
1009     M_20# = MClear%
1010     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1011     '
1012     '【IDチケット読み込み】
1013     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1014     MInspGroup%(1) = 1              '検査G番号
1015     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1016 '
1017     'エラーの場合
1018     If MRtn <> 1 Then
1019         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1020         If MRtn <> 1 Then
1021             'D720 -> D1300 コピー要求
1022             M_Out(12565) = 1
1023             Dly 0.5
1024             M_Out(12565) = 0
1025             'エラー処理記述
1026             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1027             'GOT KEY入力待ち
1028             MKeyNumber = fnKEY_WAIT()
1029             '
1030             Select MKeyNumber
1031                 Case MNext%         '次へを選択した場合
1032                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1033                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1034                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1035                     Break
1036                 Case MAbout%        '停止を選択した場合
1037                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1038                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1039                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1040                     Break
1041                 Case MNgProcess%    'NGを選択した場合
1042                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1043                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1044                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1045                     Break
1046                 Case MContinue%     '継続を選択した場合
1047                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1048                     M_20# = MContinue%
1049                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1050                     Break
1051             End Select
1052         EndIf
1053     EndIf
1054 '----------D720 -> D1300 コピー要求----------
1055     M_Out(12565) = 1
1056     Dly 0.5
1057     M_Out(12565) = 0
1058 '----------通信確認をする----------
1059     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1060     MRtn = 0                ' 初期化
1061     M_20# = MClear%         ' 初期化
1062     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1063     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1064     If MRtn <> 1 Then
1065         If M_20# = MContinue% Then
1066             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1067         Else
1068             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1069         EndIf
1070     EndIf
1071 '----------工程抜け確認----------
1072     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1073     MRtn = 0                ' 初期化
1074     M_20# = MClear%         ' 初期化
1075     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1076     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1077     If MRtn <> 1 Then
1078         If M_20# = MContinue% Then
1079             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1080         Else
1081             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1082         EndIf
1083     EndIf
1084     '
1085     fnPiasCheck = 1
1086     *fnPiasCheck_End
1087     Exit Function
1088 FEnd
1089 '
1090 '■fnPCComuCheck
1091 ''' <summary>
1092 ''' PC-PLC通信チェック
1093 ''' </summary>
1094 ''' <returns>   0 : NG
1095 '''             1 : OK(読込み完了)
1096 ''' </returns>
1097 ''' <remarks>
1098 ''' Date   : 2021/07/07 : M.Hayakawa
1099 ''' </remarks>'
1100 Function M% fnPCComuCheck
1101     fnPCComuCheck = 0
1102     MJudge% = 0                                  '初期化
1103     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1104     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1105     '
1106     For MStaNo = 0 To 5
1107         '
1108         If M_In(MIN_PIAS_ComOK%) = 1 Then
1109             'PC通信OK(M400)
1110             MJudge% = MOK%
1111             MStaNo = 5
1112             Break
1113         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1114             'toRBT_通信確認time out
1115             MJudge% = MNG%
1116             MCommentD1001 = 15
1117             MCommentD1002 = 21
1118             MStaNo = 5
1119             Break
1120         Else
1121             'toRBT_通信確認time out
1122             MJudge% = MNG%
1123             MCommentD1001 = 14
1124             MCommentD1002 = 21
1125             Break
1126         EndIf
1127     Next MStaNo
1128     '
1129     '上記で返信フラグを受信してからPC通信確認OFF
1130     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1131     '
1132     'エラー画面
1133     If MJudge% <> MOK% Then
1134         M_20# = MClear%     '初期化
1135         'エラー処理記述
1136         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1137         'GOT KEY入力待ち
1138         MKeyNumber = fnKEY_WAIT()
1139         '
1140         If MKeyNumber = MAbout% Then            '停止を選択した場合
1141             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1142             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1143             Break
1144         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1145             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1146             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1147             Break
1148         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1149             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1150             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1151             Break
1152         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1153             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1154             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1155             Break
1156         EndIf
1157     Else
1158         'OKの場合
1159         fnPCComuCheck = 1
1160     EndIf
1161     Exit Function
1162 FEnd
1163 '
1164 '■fnProcessCheck
1165 ''' <summary>
1166 ''' 工程抜け確認
1167 ''' </summary>
1168 ''' <returns>    1：工程履歴OK     0：異常終了
1169 '''             -1：前工程履歴NG  -2：自工程履歴あり
1170 '''             -3：モデル仕向NG  -4：タイムアウト
1171 '''             -5：履歴処理エラー
1172 ''' </returns>
1173 ''' <remarks>
1174 ''' Date   : 2021/07/07 : M.Hayakawa
1175 ''' </remarks>'
1176 Function M% fnProcessCheck
1177     fnProcessCheck = 0
1178     MJudge% = MNG%      '一旦NGを初期化とする
1179 '----------工程抜け確認----------
1180     MCommentD1001 = 0   'コメント初期化
1181     For MStaNo = 0 To 5
1182         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1183         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1184         '
1185         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1186             MJudge% = MOK%
1187             fnAutoScreenComment(85)     ' AUTO画面
1188             MStaNo = 5
1189             Break
1190         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1191             MFlgLoop% = 0
1192             MJudge% = MNG%
1193             MCommentD1001 = 27
1194             MCommentD1002 = 22
1195             fnAutoScreenComment(94)     ' AUTO画面
1196             fnProcessCheck = -2         ' NGは-2を返す
1197             MStaNo = 5
1198             Break
1199         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1200            MJudge% = MNG%
1201             MCommentD1001 = 31
1202             MCommentD1002 = 22
1203             fnAutoScreenComment(83)     ' AUTO画面
1204             fnProcessCheck = -3         ' NGは-3を返す
1205             MStaNo = 5
1206             Break
1207         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1208             '履歴NGは直ぐに終了せず繰り返し確認を行う
1209             '前工程の書込みが終了していない可能性があるため
1210             MJudge% = MNG%
1211             MCommentD1001 = 32
1212             MCommentD1002 = 22
1213             fnAutoScreenComment(84)     ' AUTO画面
1214             fnProcessCheck = -1         ' NGは-1を返す
1215             Dly 1.0
1216             '工程抜け確認OFF
1217             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1218             Dly 1.0
1219            'MStaNo = 5
1220             Break
1221         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1222             MFlgLoop% = 0
1223             MJudge% = MNG%
1224             MCommentD1001 = 29
1225             MCommentD1002 = 22
1226             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1227             fnProcessCheck = -5         ' NGは-5を返す
1228             MStaNo = 5
1229             Break
1230         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1231             MJudge% = MNG%
1232             If MCommentD1001 = 32 Then
1233                 '何もしない
1234             Else
1235                 MCommentD1001 = 26
1236             EndIf
1237             MCommentD1002 = 22
1238             fnProcessCheck = -4         ' NGは-4を返す
1239             MStaNo = 5
1240             Break
1241         Else
1242             MJudge% = MNG%
1243             MCommentD1001 = 28
1244             MCommentD1002 = 22
1245         EndIf
1246     Next MStaNo
1247     '工程抜け確認OFF
1248     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1249     '通過履歴NG 工程抜けの場合
1250     If MJudge% = MPass% Then
1251         M_20# = MPass%
1252     EndIf
1253     '
1254     'エラー画面
1255     If MJudge% <> MOK% Then
1256         M_20# = MClear%     '初期化
1257         'エラー処理記述
1258         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1259         'GOT KEY入力待ち
1260         MKeyNumber = fnKEY_WAIT()
1261         '
1262         Select MKeyNumber
1263             Case MAbout%        '停止を選択した場合
1264                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1265                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1266                 Break
1267             Case MNext%         '次へを選択した場合
1268                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1269                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1270                 Break
1271             Case MContinue%     '継続を選択した場合
1272                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1273                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1274                 Break
1275             Case MNgProcess%    'NGを選択した場合
1276                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1277                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1278                 Break
1279         End Select
1280     Else
1281         fnProcessCheck = 1  ' OKは1を返す
1282     EndIf
1283     Exit Function
1284 FEnd
1285 '
1286 '■fnPiasWrite
1287 ''' <summary>
1288 ''' Pias 組立結果書込み要求
1289 ''' </summary>
1290 '''<param name="MFlg%">
1291 '''                 MOK%(1) = 工程履歴にOKを書込む
1292 '''                 MNG%(0) = 工程履歴にNGを書込む
1293 '''</param>
1294 '''<returns></returns>
1295 ''' <remarks>
1296 ''' Date   : 2021/07/07 : M.Hayakawa
1297 ''' </remarks>'
1298 Function M% fnPiasWrite(ByVal MFlg%)
1299       fnPiasWrite = 0
1300 *RETRY_PIASWRITE
1301     '
1302     '組立OK(MOK%)の場合　M306 ON
1303    '組立NG(MNG%)の場合　M307 ON
1304     If MFlg% = MOK% Then
1305         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1306     Else
1307         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1308     EndIf
1309     Dly 0.1                  '念のため
1310     '
1311     'Piasへ書込み開始 M305 -> ON
1312     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1313     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1314     '
1315     MJudge% = MNG%
1316     '
1317     For MStaNo = 0 To 5
1318         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1319             MJudge% = MOK%
1320             'MRet = fnAutoScreenComment(85)  'AUTO画面
1321             MStaNo = 5
1322             Break
1323         '
1324         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1325             MJudge% = MNG%
1326             'MRet = fnAutoScreenComment(85)  'AUTO画面
1327            MCommentD1001 = 34
1328            MCommentD1002 = 25
1329             MStaNo = 5
1330             Break
1331         '
1332         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1333             MJudge% = MNG%
1334             'MRet = fnAutoScreenComment(85)  'AUTO画面
1335            MCommentD1001 = 35
1336            MCommentD1002 = 25
1337             MStaNo = 5
1338             Break
1339         '
1340         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1341             MJudge% = MNG%
1342             'MRet = fnAutoScreenComment(85)  'AUTO画面
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
1357     'Piasへ書込み開始 M305 -> OfF
1358     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1359     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1360     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1361     '
1362     '
1363     '通過履歴NG 工程抜けの場合
1364     If MJudge% = MPass% Then
1365         M_20# = MPass%
1366     EndIf
1367     '
1368    M_20# = MClear%     '初期化
1369     '
1370     'エラー画面
1371     If MJudge% < MOK% Then
1372     '
1373 '残しておくが現状では使用しないラベル
1374 *RETRY_ERR_WRITE
1375         M_20# = MClear%     '初期化
1376         'エラー処理記述
1377         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1378         'GOT KEY入力待ち
1379         MKeyNumber = fnKEY_WAIT()
1380         '
1381         If MKeyNumber = MAbout% Then   '停止を選択した場合
1382             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1383            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1384             Break
1385         '
1386         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1387             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1388             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1389         '
1390         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1391             M_20# = MPass%            'M_20# プログラム間共通外部変数
1392             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1393         '
1394         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1395             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1396            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
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
1411 '■fnPCBNumberCheck
1412 ''' <summary>
1413 ''' Pias 基板番号照合要求
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
1425     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1426     'Piasへ基板照合開始 M310 -> ON
1427     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1428     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1429     '
1430     MJudge% = MNG%
1431     '
1432     For MStaNo = 0 To 5
1433         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1434             MJudge% = MOK%
1435             fnAutoScreenComment(96)  'AUTO画面
1436             MStaNo = 5
1437             Break
1438         '
1439         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1440             MJudge% = MNG%
1441             fnAutoScreenComment(97)  'AUTO画面
1442             MCommentD1001 = 37
1443             MCommentD1002 = 25
1444             MStaNo = 5
1445             Break
1446         '
1447         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1448             MJudge% = MNG%
1449             fnAutoScreenComment(98)  'AUTO画面
1450             MCommentD1001 = 38
1451             MCommentD1002 = 25
1452             MStaNo = 5
1453             Break
1454         '
1455         ElseIf M_In(11580) = 1 Then                         'time out
1456             MJudge% = MNG%
1457             fnAutoScreenComment(99)  'AUTO画面
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
1472     'Piasへ基板照合開始 M310 -> OfF
1473     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1474     '
1475     '
1476     '通過履歴NG 工程抜けの場合
1477     If MJudge% = MPass% Then
1478         M_20# = MPass%
1479     EndIf
1480     '
1481    M_20# = MClear%     '初期化
1482     '
1483     'エラー画面
1484     If MJudge% < MOK% Then
1485     '
1486 '残しておくが現状では使用しないラベル
1487 *RETRY_ERR_PCBNUMBER
1488         M_20# = MClear%     '初期化
1489         'エラー処理記述
1490         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1491         'GOT KEY入力待ち
1492         MKeyNumber = fnKEY_WAIT()
1493         '
1494         If MKeyNumber = MAbout% Then   '停止を選択した場合
1495             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1496             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1497             Break
1498         '
1499         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1500             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1501             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1502         '
1503         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1504             M_20# = MPass%            'M_20# プログラム間共通外部変数
1505             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1506         '
1507         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1508             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1509             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
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
1522 '■ScrewTight
1523 ''' <summary>
1524 ''' ねじ締めを行う(Sタイト)
1525 ''' </summary>
1526 '''<param name="PScrewPos()">
1527 '''             PScrewPos(1)    ：パレット上ねじ締めS?@の安全回避位置  +30
1528 '''             PScrewPos(2)    ：ねじ締め回避点
1529 '''             PScrewPos(10)   ：ねじ締め終了高さ
1530 '''<param name="MScrewType">ネジタイプ(mm/sec)
1531 '''             1:6mm Sタイト銀ネジ
1532 '''             2:8mm Pタイト
1533 '''             3:6mm Sタイト黒ネジ
1534 '''             4:13mm Sタイト
1535 '''             5:6mm Mネジ
1536 '''</param>
1537 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
1538 '''<returns>整数
1539 '''         0=異常終了、1=正常終了
1540 '''</returns>
1541 ''' <remarks>
1542 ''' Date   : 2021/07/07 : M.Hayakawa
1543 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
1544 ''' </remarks>'
1545 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
1546     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1547     ScrewTight = 0
1548     MOKNGFlg = 0
1549     Ovrd 100
1550     Mov PScrewPosition(1)       ' パレット上ねじ締めS?@の安全回避位置
1551     Fine 0.05 , P
1552     Ovrd MOvrdA%
1553     ' 減速設定
1554     Accel 100, 10
1555     ' パレット上ねじ締め開始位置へ移動
1556     Mvs PScrewPosition(2)
1557     ' 加減速を元に戻す
1558     Accel
1559     ' 内部Ovrd設定
1560 '    Ovrd MOvrdA%
1561     Ovrd 100
1562     ' Spd設定
1563 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
1564     Spd MFeedSpd
1565     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
1566     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
1567     Select MScrewType%
1568         Case 1
1569             ' Sタイト：プログラム1、バンク1に設定
1570             ProgramBankSet(1,1)
1571             Break
1572         Case 2
1573             ' Pタイト：プログラム1、バンク1に設定
1574             ProgramBankSet(3,1)
1575             Break
1576         Case 3
1577             ' Sタイト黒：プログラム1、バンク1に設定
1578             ProgramBankSet(1,1)
1579             Break
1580         Case 4
1581             ' Sタイト13mm：プログラム1、バンク1に設定
1582             ProgramBankSet(1,1)
1583             Break
1584         Case 5
1585             ' Mネジ：プログラム1、バンク1に設定
1586             ProgramBankSet(1,1)
1587             Break
1588         Case 6
1589             ' Sタイト：プログラム1、バンク4に設定
1590             ProgramBankSet(1,4)
1591             Break
1592         Default
1593             ' プログラム1、バンクなし設定
1594             ProgramBankSet(0,0)
1595             Break
1596     End Select
1597 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
1598      'ドライバーON　CW
1599     M_Out(12241)=1
1600     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1601     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
1602     Dly 0.1
1603     Fine 0 , P
1604     Spd M_NSpd
1605     '
1606     If M_In(11256)=1 Then  'ねじトータルエラー検出時
1607         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
1608         Dly 0.1
1609        ' プログラム・バンク解除
1610         ProgramBankSet(0,0)
1611         'パレット上ねじ締め終了位置上空へ移動
1612         Mvs PScrewPosition(10),-80
1613         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1614         M_Out(12249)=1 Dly 0.3
1615         MOKNGFlg = -1
1616         ScrewTight = 0
1617     Else
1618          'ドライバーOFF　CW
1619         M_Out(12241)=0
1620 '        エラーがない場合はネジ締め終了位置で増し締め
1621 '        Select MScrewType%
1622 '            Case 1
1623 '                ' Sタイト：プログラム1、バンク3に設定
1624 '                ProgramBankSet(1,3)
1625 '                Break
1626 '            Case 2
1627 '                ' Pタイト：プログラム1、バンク3に設定
1628 '                ProgramBankSet(3,3)
1629 '                Break
1630 '            Case 3
1631 '                ' Sタイト黒：プログラム1、バンク3に設定
1632 '                ProgramBankSet(1,3)
1633 '                Break
1634 '            Case 4
1635 '                ' Sタイト13mm：プログラム1、バンク3に設定
1636 '                ProgramBankSet(1,3)
1637 '                Break
1638 '            Case 5
1639 '                ' Mネジ：プログラム1、バンク3に設定
1640 '                ProgramBankSet(1,3)
1641 '                Break
1642 '            Default
1643 '                ' プログラム1、バンクなし設定
1644 '                ProgramBankSet(0,0)
1645 '                Break
1646 '        End Select
1647 '         'ドライバーON　CW
1648 '        Mvs PScrewPosition(10)
1649 '        M_Out(12241)=1
1650 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
1651 '
1652          'ドライバーOFF　CW
1653         M_Out(12241)=0
1654        ' プログラム・バンク解除
1655         ProgramBankSet(0,0)
1656         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1657         M_Out(12249)=1 Dly 0.3
1658     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
1659         'パレット上ねじ締め終了位置上空へ移動
1660        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1661         'Mvs PScrewPosition(10),-80
1662         ScrewTight = 1
1663     EndIf
1664 ' 暫定（暫定マスク　9/16 M.Hayakawa)
1665 '    Ovrd 10
1666 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS?@の安全回避位置
1667     Ovrd 100
1668     Exit Function
1669 FEnd
1670 '
1671 '■ScrewGet
1672 ''' <summary>
1673 ''' ねじ供給機からねじを得る
1674 ''' </summary>
1675 '''<param name="%">
1676 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
1677 '''         PScrewPos(2)    ：ねじ供給器回避点
1678 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
1679 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1680 '''         PScrewPos(3)    ：Mねじポカヨケ位置
1681 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
1682 '''</param>
1683 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
1684 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
1685 '''<returns>整数
1686 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
1687 '''</returns>
1688 ''' <remarks>
1689 ''' Date   : 2021/07/07 : M.Hayakawa
1690 ''' </remarks>
1691 '''<update>
1692 '''Date    : 2021/11/15 : 中村
1693 '''</update>
1694 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1695     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
1696     ScrewGet = 0
1697     MScrewJudge% = 0
1698     MFinCnt% = 2
1699     'ねじ供給器初期動作エラーチェック
1700 ' ↓暫定削除
1701     'Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
1702     For MCnt% = 0 To MFinCnt%
1703         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
1704         If MRtn = 0 Then
1705             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1706             ScrewGet = -1
1707             MScrewJudge% = 2
1708         EndIf
1709         Ovrd 100
1710         If FeederScrewSensor% <> 0 Then
1711             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
1712                 'Ovrd 30
1713                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
1714                 'NGとしてここの関数から抜ける
1715                 ScrewGet = -2
1716                 MScrewJudge% = 3
1717             EndIf
1718         EndIf
1719         Ovrd 100
1720         Spd M_NSpd
1721         If MScrewJudge% = 0 Then
1722     '        ScrewGet = 0
1723             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
1724             MScrewCnt% = 0
1725             MFinCnt% = 2
1726             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
1727             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1728             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
1729             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
1730             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
1731             'Mvs PScrewPosition(10), 1.2
1732             'ビット回転
1733             M_Out(Y60_Driver)=1
1734             Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
1735             M_Timer(4) = 0
1736             MloopFlg = 0
1737             MCntTime& = 0
1738             While MloopFlg = 0
1739                 MCrtTime& = M_Timer(4)
1740                 If MCrtTime& >= 180 Then
1741                     MloopFlg = 1
1742                 EndIf
1743             WEnd
1744             M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
1745             '吸着確認
1746             MRtn = 0
1747             MRtn = frInCheck(11271, 1, MSETTIMEOUT01&)
1748             '
1749             JOvrd M_NJovrd
1750             Spd M_NSpd
1751             'ネジ吸着確認位置移動
1752             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
1753             Mvs PScrewPosition(10), -30  ' ネジ吸着確認位置
1754            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1755             'ビット回転停止
1756             M_Out(Y60_Driver)=0
1757             '
1758             '1秒間ネジ吸着確認 始めの閾値
1759 '            If MRtn = 1 Then           　'シーケンス変更につきコメントアウト(5/13中村)
1760                 MRtn = frInCheck(11272, 1, MSETTIMEOUT03&)
1761 '            EndIf                      　'シーケンス変更につきコメントアウト(5/13中村)
1762             'MRtn = 0'強制エラー
1763             '吸着エラーの場合
1764             'ネジをねじ太郎に戻す
1765             If MRtn = 0 Then
1766                 Ovrd 30      '2から5に変更
1767                 'ビット回転停止
1768                 M_Out(Y60_Driver)=0
1769                 'ネジ供給機上空
1770                 Mvs PScrewPosition(1)
1771                 '更に上空
1772                 Mov PScrewPosition(1), -140
1773                 'ネジ捨て位置
1774                 MRtn = FnCtlValue2(4)          '吸着エラー数＋１  2022/04/28 渡辺
1775                 Mov PScrewPosition(9)
1776                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
1777                 '吸着OFF
1778                 M_Out(12249)=1 Dly 0.3 'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1779                 Dly 0.2
1780                 '破壊ON
1781                 M_Out(Y6B_VB1)=1 '真空破壊ON
1782                 'ビット回転
1783                 M_Out(Y61_Driver)=1
1784                 Dly 0.5
1785                 '                '
1786                 Ovrd 100
1787                 JOvrd M_NJovrd
1788                 Spd M_NSpd
1789                 'ドライバーを上下させねじを振り落とす
1790                 Mov PScrewPosition(9), 10
1791                 Mov PScrewPosition(9)
1792                 Dly 0.1
1793                 Mov PScrewPosition(9), 10
1794                 Mov PScrewPosition(9)
1795                 '
1796                 'ネジ落ち待ち
1797                 Wait M_In(11272) = 0
1798                 'ビット回転停止
1799                 M_Out(Y61_Driver)=0
1800                 Dly 0.1
1801                 '破壊OFF
1802                 M_Out(Y6B_VB1)=0 '真空破壊OFF
1803                 'ねじ落ちたとして、移動更に上空
1804                 Mov PScrewPosition(1), -140
1805                 Ovrd 100
1806                 Spd M_NSpd
1807                 'ネジ供給機上空
1808                 Mvs PScrewPosition(1)
1809 '                '
1810                 ScrewGet = -3
1811                 If MCnt% = MFinCnt% Then
1812                     MScrewJudge% = 4
1813                     Mov PScrewPosition(2)
1814                     Break
1815                 EndIf
1816                 Break
1817 '                '
1818             Else
1819                 MCnt% = MFinCnt%
1820                 ScrewGet = 1
1821             EndIf
1822         Else
1823             MCnt% =MFinCnt%
1824         EndIf
1825     Next  MCnt%
1826         '
1827 '    If MScrewJudge% = 0 Then
1828 '        Ovrd 100
1829 '        Spd M_NSpd
1830 '        PScrewPosition(1)
1831 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1832 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1833 '        M_Out(Y60_Driver)=0     ' ビット回転停止
1834 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
1835 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
1836 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
1837 '        'Mov PScrewPosition(2)
1838 '        'もう一度吸着確認　上空の最終閾値
1839 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
1840 '        If MRtn = 0 Then      '吸着エラーの場合
1841 '            MScrewJudge% = 4
1842 '            ScrewGet = -3
1843 '        ElseIf MRtn = 1 Then      '吸着OKの場合
1844 '            MScrewJudge% = 1
1845 '            ScrewGet = 1
1846 '        EndIf
1847 '        Break
1848 '    EndIf
1849     '
1850 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1851     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
1852     '
1853     Select MScrewJudge%
1854 '        Case 0
1855 ''            fErrorProcess(11,162,163,0) '異常終了
1856 '            MCommentD1001 = 162
1857 '            MCommentD1002 = 96
1858 '            Break
1859         Case 2
1860 '            fErrorProcess(11,63,161,0) '供給NG
1861             MCommentD1001 = 63
1862             MCommentD1002 = 96
1863             Break
1864         Case 3
1865 '            fErrorProcess(11,160,164,0) '誤供給
1866             MCommentD1001 = 237
1867             MCommentD1002 = 96
1868             Break
1869         Case 4
1870 '            fErrorProcess(11,94,95,0) '吸着NG
1871             MCommentD1001 = 94
1872             MCommentD1002 = 95
1873             Break
1874     End Select
1875     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
1876     '
1877     Select M_20#
1878         Case MAbout%          '停止が押された場合
1879             Mov PScrewPosition(2)                  '初期位置に戻って停止処理
1880             Mov PInitialPosition
1881             Break
1882         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
1883             Break
1884         Case MNext%           '継続が押された場合
1885             M_20# = MClear%     '初期化
1886             Break
1887         Case MNgProcess%      'NGが押された場合
1888             Mov PScrewPosition(2)   'PIASにNG書き込みを行い,初期位置に戻って行程終了
1889             Mov PInitialPosition
1890             Break
1891         End Select
1892 *End_ScrewGet
1893     Exit Function
1894 FEnd
1895 '
1896 '■ProgramBankSet
1897 ''' <summary>
1898 ''' ねじ締めを行う(Pタイト)
1899 ''' </summary>
1900 '''<param name="MProgramNo">プログラム番号</param>
1901 '''<param name="MBankNo">バンク番号</param>
1902 '''</returns>
1903 ''' <remarks>
1904 ''' Date   : 2021/10/05 : M.Hayakawa
1905 ''' </remarks>'
1906 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1907 '
1908     MLocalPrgNo% = (MProgramNo% - 1) * 32
1909     MLocalBankNo% = MBankNo% * 4
1910 '
1911     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
1912         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
1913     Else
1914         MLocalOutNo% = 0
1915     EndIf
1916 '
1917     M_Out8(12240) = MLocalOutNo%
1918     Dly 0.1
1919     Exit Function
1920 FEnd
1921 '
1922 '■fnKEY_WAIT()
1923 ''' <summary>
1924 ''' GOTからのキー入力待ち
1925 ''' </summary>
1926 '''<returns>1：停止    2：次へ
1927 '''         3：継続    4：トルクチェック開始
1928 '''         5：NG
1929 '''         11：ロボット初期位置1    12：ロボット初期位置2
1930 '''         13：ロボット初期位置3    14：ロボット初期位置4
1931 '''</returns>
1932 ''' <remarks>
1933 ''' Date   : 2021/07/07 : M.Hayakawa
1934 ''' </remarks>'
1935 Function M% fnKEY_WAIT()
1936     fnKEY_WAIT = 0
1937     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
1938     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
1939     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
1940     '下記キー待ちの継続に反応させないため
1941     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
1942     Dly 0.2
1943     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
1944     MLocalLoopFlg=1
1945     While MLocalLoopFlg=1
1946         If M_In(11345) = 1 Then         '停止   M5345
1947             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
1948             fnKEY_WAIT = 1
1949             MLocalLoopFlg=-1
1950             Break
1951         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
1952             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
1953             fnKEY_WAIT = 2
1954             MLocalLoopFlg=-1
1955             Break
1956         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
1957             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
1958             fnKEY_WAIT = 3
1959             MLocalLoopFlg=-1
1960             Break
1961         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
1962             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
1963             fnKEY_WAIT = 4
1964             MLocalLoopFlg=-1
1965             Break
1966         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
1967             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
1968             fnKEY_WAIT = 5
1969             MLocalLoopFlg=-1
1970             Break
1971             '
1972         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
1973             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
1974             fnKEY_WAIT = MRobotInit1%
1975             MLocalLoopFlg=-1
1976             Break
1977             '
1978         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
1979             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
1980             fnKEY_WAIT = MRobotInit2%
1981             MLocalLoopFlg=-1
1982             Break
1983             '
1984         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
1985             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
1986             fnKEY_WAIT = MRobotInit3%
1987             MLocalLoopFlg=-1
1988             Break
1989             '
1990         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
1991             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
1992             fnKEY_WAIT = MRobotInit4%
1993             MLocalLoopFlg=-1
1994             Break
1995             '
1996         Else
1997         EndIf
1998     WEnd
1999     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2000     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2001     Exit Function
2002 FEnd
2003 '
2004 '■ fnAUTO_CTL
2005 ''' <summary>
2006 ''' AUTOモードOFF、PLCからの開始待ち
2007 ''' </summary>
2008 ''' <remarks>
2009 ''' Date   : 2021/07/07 : M.Hayakawa
2010 ''' </remarks>
2011 Function M% fnAUTO_CTL
2012     fnAUTO_CTL = 0
2013     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2014     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2015     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2016     '
2017     If M_Svo=0 Then             'サーボON確認
2018         Servo On
2019     EndIf
2020     Wait M_Svo=1
2021     Exit Function
2022 FEnd
2023 '
2024 '■ fnWindScreenOpen
2025 ''' <summary>
2026 ''' ウィンド画面の表示、非表示設定
2027 ''' </summary>
2028 '''<param name="%"></param>
2029 '''<param name="%"></param>
2030 '''<param name="%"></param>
2031 '''<param name="%"></param>
2032 ''' <remarks>
2033 ''' コメントD1001, D1002, D1003の設定
2034 ''' MWindReSet = 0     画面非表示
2035 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2036 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2037 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2038 ''' Date   : 2021/07/07 : M.Hayakawa
2039 ''' </remarks>
2040 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2041     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2042         M_Out16(12480) = MCommentD1001            'D1001 コメント
2043     EndIf
2044     '
2045     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2046         M_Out16(12496) = MCommentD1002            'D1002 コメント
2047     EndIf
2048     '
2049     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2050        M_Out16(12512) = MCommentD1003            'D1003 コメント
2051     EndIf
2052     '
2053     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2054     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2055     Dly 0.5
2056     M_Out(12363) = 0                         'ウィンド画面設定
2057     Exit Function
2058 FEnd
2059 '
2060 '■FnCtlValue2
2061 ''' <summary>
2062 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2063 ''' </summary>
2064 ''' <param name="MCtlNo%"></param>
2065 ''' <remarks>
2066 ''' Date : 2022/04/28 渡辺
2067 ''' </remarks>
2068 '''
2069 '''  1：投入数       ＋１
2070 '''  2：組立ＯＫ数   ＋１
2071 '''  3：組立ＮＧ数   ＋１ (未使用)
2072 '''  4：吸着エラー数 ＋１
2073 ''' 99：読書開始信号 OFF
2074 '''
2075 Function M% FnCtlValue2(ByVal MCtlNo%)
2076     FnCtlValue2 = 1
2077     Select MCtlNo%
2078         Case 1        '投入数＋１
2079             M_Out(12569) = 0             '書込み開始信号OFF
2080             M_Out(12568) = 1             '読込み開始信号ON
2081             MInputQty = M_In16(11600)    '投入数受信
2082             MInputQty = MInputQty + 1    '投入数＋１
2083             M_Out16(12592) = MInputQty   '投入数送信
2084             M_Out(12569) = 1             '書込み開始信号ON
2085             Break
2086             '
2087         Case 2        '組立ＯＫ数＋１
2088             M_Out(12569) = 0             '書込み開始信号OFF
2089             M_Out(12568) = 1             '読込み開始信号ON
2090             MAssyOkQty = M_In16(11616)   '組立OK数受信
2091             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2092             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2093             M_Out(12569) = 1             '書込み開始信号ON
2094             Break
2095             '
2096         Case 4        '吸着エラー数＋１
2097             M_Out(12569) = 0                       '書込み開始信号OFF
2098             M_Out(12568) = 1                       '読込み開始信号ON
2099             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2100             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2101             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2102             M_Out(12569) = 1                       '書込み開始信号ON
2103             Break
2104             '
2105         Case 99        '読書開始信号OFF
2106             M_Out(12568) = 0        '読込み開始信号OFF
2107             M_Out(12569) = 0        '書込み開始信号OFF
2108             Break
2109             '
2110     End Select
2111     Exit Function
2112 FEnd
2113 '
2114 '
2115 '■FnScreEroorCord
2116 ''' 電動ドライバーのエラーコードを含めたコメントを出す為のコメント番号の作成
2117 ''' 新規作成：2022/05/23 : 渡辺
2118 '''
2119 Function M% FnScreEroorCord()
2120     MScrewErrorCord% = 0
2121     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2122     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2123     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2124     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2125     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2126     MScrewErrorCord% = MScrewErrorCord% * 10
2127     MScrewErrorCord% = MScrewErrorCord% + 500
2128     FnScreEroorCord = MScrewErrorCord%
2129     Exit Function
2130 FEnd
2131 '
2132 '
2133 'Insightによる画像処理検査実行（並列処理なし）
2134 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2135 '-------------------------------------------------------------------------------
2136 'Insightによる画像処理検査実行（並列処理なし）
2137 '   引数
2138 '       PInspPos()      ：検査位置
2139 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2140 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2141 '       MInspCnt%       ：検査位置数
2142 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2143 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2144 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2145 '   戻り値：整数
2146 '       0=異常終了、1=正常終了
2147 '
2148 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2149 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2150 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2151 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2152 '   20200410    :   検査グループ設定Retry追加
2153 '-------------------------------------------------------------------------------
2154     '----- 初期設定 -----
2155     Cnt 0                                                           '移動効率化解除(初期値=0)
2156     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2157 '    Cnt 1,0.1,0.1
2158     '変数宣言・初期化
2159     Def Inte MNum                                                   '検査番号(検査順1〜)
2160     MNum% = 1                                                       '検査番号初期値設定
2161     Def Inte MEndFlg                                                '検査終了フラグ
2162     MEndFlg% = 0
2163     '
2164     '検査G番号設定要求・検査実行要求off
2165     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2166     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2167     'エラー番号クリア
2168     MInspErrNum = 0                                                 '検査実行エラー番号
2169     M_Out16(MOUT_InspErrNum) = MInspErrNum
2170     MInspNGStepNum = 0                                              '検査実行NGStep番号
2171     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2172     '
2173     'Insight Ready check?
2174     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2175         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2176         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2177         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2178         ISInspectionSingle = 0                                      '異常終了戻り値設定
2179         Exit Function
2180     EndIf
2181     '
2182     '検査位置数確認
2183     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2184         MInspErrNum = 21                                            '検査データなし 21　引数<1
2185         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2186         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2187         ISInspectionSingle = 0                                      '異常終了戻り値設定
2188         Exit Function
2189     EndIf
2190     '
2191     '
2192     '
2193     '----- メイン処理 -----
2194     '設定された検査位置数分の検査実行
2195     While( MEndFlg% = 0 )
2196         '----- 検査グループ番号設定Retry追加 20200410
2197         MSetGrNumRetryExitFlg = 0
2198         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2199         While( MSetGrNumRetryExitFlg = 0 )
2200         '----- 検査グループ番号設定Retry追加ここまで 20200410
2201             '
2202             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2203             '
2204             '----- 検査グループ番号設定 -----
2205             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2206             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2207             '
2208             '検査位置へ移動・移動完了待ち
2209             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2210             Mvs PInspPos( MNum% )                                       '移動
2211             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2212             Dly 0.05                                                    '移動完了後Delay
2213             '
2214             '検査グループ番号設定終了確認
2215             M_Timer(1) = 0
2216             MExitFlg = 0
2217             While( MExitFlg = 0 )
2218                 '検査G設定正常終了?
2219                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2220                     MExitFlg = 1
2221                 '
2222                 '検査G設定異常終了?
2223                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2224                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2225                     If MInspErrNum = 0 Then                             '1回目のエラー?
2226                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2227                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2228                     EndIf
2229                     MExitFlg = 1
2230                 '
2231                 'timeoutチェック
2232                 ElseIf 1000 < M_Timer(1) Then
2233                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2234                     If MInspErrNum = 0 Then                             '1回目のエラー?
2235                         MInspErrNum = 12                                'timeout エラー番号=12
2236                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2237                     EndIf
2238                     MExitFlg = 1
2239                 EndIf
2240             WEnd
2241             '
2242             '検査G番号設定要求off
2243             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2244             '
2245             '----- 検査グループ設定Retry追加 20200410
2246             'NGなければ抜ける
2247             If MCurrentStepErr = 0 Then
2248                 MSetGrNumRetryExitFlg = 1
2249             Else
2250                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2251                 If MSetGrNumRetryCnt = 0 Then
2252                     MSetGrNumRetryExitFlg = 1
2253                 Else
2254                     'Retryへ　その前にDelay
2255                     Dly 0.5
2256                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2257                 EndIf
2258             EndIf
2259             '----- 検査グループ設定Retry追加ここまで 20200410
2260             '
2261         WEnd
2262         '
2263         '
2264         '
2265         '----- 検査実行 -----
2266         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2267             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2268                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2269                 MInspRetryExitFlg = 0
2270                 MRetryCnt = 2                                        'Retry回数設定
2271                 While( MInspRetryExitFlg = 0 )
2272                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2273                     '
2274                     '検査完了確認
2275                     MRetryCnt = MRetryCnt - 1
2276                     M_Timer(1) = 0
2277                     MExitFlg = 0
2278                     While( MExitFlg = 0 )
2279                     '検査完了待ち
2280                         '検査OK終了?
2281                         If M_In( MIN_IS_InspOK% ) = 1  Then
2282                             MJudgeOKFlg = 1                         '検査OKフラグON
2283                             MExitFlg = 1
2284                         '
2285                         '検査NG終了?
2286                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2287                             If MInspErrNum = 0 Then                 '1回目のエラー?
2288                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2289                                     MInspErrNum = 32                    '検査NG エラー番号=32
2290                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2291                                 EndIf
2292                             EndIf
2293                             MExitFlg = 1
2294                         '
2295                         '検査異常終了(IS timeout)?
2296                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2297                             If MInspErrNum = 0 Then                 '1回目のエラー?
2298                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2299                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2300                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2301                                 EndIf
2302                             EndIf
2303                             MExitFlg = 1
2304                         '
2305                         'timeoutチェック
2306                         ElseIf 3000 < M_Timer(1) Then
2307                             If MInspErrNum = 0 Then                 '1回目のエラー?
2308                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2309                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2310                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2311                                 EndIf
2312                             EndIf
2313                             MExitFlg = 1
2314                         EndIf
2315                     WEnd
2316                     '
2317                     '検査開始要求off
2318                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2319                     '
2320                     'OKなら抜ける
2321                     If MJudgeOKFlg = 1 Then
2322                         MInspRetryExitFlg = 1
2323                     Else
2324                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2325                         If MRetryCnt = 0 Then
2326                             MInspRetryExitFlg = 1
2327                         Else
2328                             'Retryへ　その前にDelay
2329                             Dly 0.3
2330                         EndIf
2331                     EndIf
2332                     '
2333                 WEnd
2334             EndIf
2335         EndIf
2336         '
2337         '
2338         '
2339         MNum% = MNum% + 1                                           '検査Step+1
2340         '検査終了確認　検査終了フラグセット
2341         If (MInspCnt% < MNum% ) Then
2342             MEndFlg% = 1                                            '検査終了フラグセット
2343         EndIf
2344         'NG発生時続行時処理
2345         If MInspErrNum <> 0 Then                                    'NGあり?
2346             If MNgContinue% <> 1 Then                               'NG続行?
2347                 MEndFlg% = 1                                        '検査終了フラグセット
2348             EndIf
2349         EndIf
2350     WEnd
2351     '
2352     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2353     If 0 < MZAxis% Then
2354         PCurrentPos = P_Curr                                        '現在位置取得
2355         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2356         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2357         Mvs PCurrentPos                                             '現在位置上空へ移動
2358     EndIf
2359     '
2360     '戻り値設定
2361     If MInspErrNum = 0 Then
2362         ISInspectionSingle = 1                                      '正常終了戻り値設定
2363     Else
2364         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2365         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2366         ISInspectionSingle = 0                                      '異常終了戻り値設定
2367     EndIf
2368     Fine 0 , P
2369     Exit Function
2370 FEnd
2371 '
2372 '■fnAutoScreenComment
2373 ''' <summary>
2374 ''' メイン画面の動作状況表示
2375 ''' コメントD1005の設定
2376 ''' </summary>
2377 '''<param name="McommentD1005%">コメントID</param>
2378 ''' <remarks>
2379 ''' Date   : 2021/07/07 : M.Hayakawa
2380 ''' </remarks>
2381 Function fnAutoScreenComment(ByVal McommentD1005%)
2382     M_Out16(12576) = McommentD1005%
2383     Exit Function
2384 FEnd
2385 '
2386 '■fnRoboPosChk
2387 ''' <summary>
2388 ''' 最後に終了したロボットポジションの確認
2389 ''' </summary>
2390 '''<param name="MINNumber%">入力番号</param>
2391 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2392 '''<param name="MTimeCnt&">タイムアウト時間</param>
2393 ''' PLCに保続した番号を読込み、確認
2394 ''' MRBTOpeGroupNo = 5 が初期位置に設定
2395 '''<returns>整数 0:タイムアウト 1:OK</returns>
2396 ''' <remarks>
2397 ''' Date   : 2021/07/07 : M.Hayakawa
2398 ''' </remarks>
2399 Function M% fnRoboPosChk
2400     fnRoboPosChk = 0
2401     MRet = fnStepRead()
2402     '初期位置でないと判断した場合
2403     'ウィンド画面切換え
2404     If MRBTOpeGroupNo > 5 Then
2405         '下記キー待ちの継続に反応させないため
2406         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2407         Dly 0.2
2408         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2409         Dly 1.5
2410         '
2411         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
2412         '
2413         MLoopFlg% = 1
2414         While MLoopFlg% = 1
2415             '
2416             '
2417             MKeyNumber% = fnKEY_WAIT()
2418             Select MKeyNumber%
2419                 Case Is = MAbout%       '停止
2420                     M_20# = MAbout%
2421                     MLoopFlg% = -1
2422                     Break
2423                 Case Is = MNext%        '次へ
2424                     'MLoopFlg% = -1
2425                     Break
2426                 Case Is = MContinue%    '継続
2427                     M_20# = MContinue%
2428                     MLoopFlg% = -1
2429                     Break
2430                 Default
2431                     Break
2432             End Select
2433         WEnd
2434     EndIf
2435     '
2436     If M_20# = MContinue% Then                              '継続ボタンが押された場合
2437         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
2438         Ovrd 5                                   '低速オーバーライド値設定
2439         Select MRBTOpeGroupNo
2440             Case Is = 5                          '何もしない
2441                 Break
2442             Case Is = 10                         '初期位置へ戻す
2443                 'Mov PTEST001
2444                 Break
2445             Case Is = 15                         '初期位置へ戻す
2446                 'Mov PTEST002
2447                 Dly 0.5
2448                 'Mov PTEST001
2449                 Dly 0.5
2450                 Break
2451             Default
2452                 Break
2453         End Select
2454         '
2455         Ovrd M_NOvrd                            'システムの初期値を設定
2456         M_Out(12364) = 1                        'toPLC_データ保存ON
2457         MRBTOpeGroupNo = 5
2458         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
2459         Dly 1.0
2460         M_Out(12364) = 0                        'toPLC_データ保存OFF
2461         fnRoboPosChk = 1                        '初期位置動作実行
2462         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
2463     EndIf
2464     Exit Function
2465 FEnd
2466 '
2467 '■frInCheck
2468 ''' <summary>
2469 ''' センサーINチェック
2470 ''' </summary>
2471 '''<param name="MINNumber%">入力番号</param>
2472 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
2473 '''<param name="MTimeCnt&">タイムアウト時間</param>
2474 '''<returns>整数 0:タイムアウト 1:OK</returns>
2475 ''' <remarks>
2476 ''' Date   : 2021/07/07 : M.Hayakawa
2477 ''' </remarks>
2478 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2479     M_Timer(4) = 0
2480     MloopFlg = 0
2481     While MloopFlg = 0
2482         MCrtTime& = M_Timer(4)
2483         If M_In(MINNumber%) = MCMPFLG% Then
2484             MloopFlg = 1
2485             frInCheck = 1
2486         ElseIf MCrtTime& > MTimeCnt& Then
2487             MloopFlg = 1
2488             frInCheck = 0
2489         EndIf
2490     WEnd
2491     Exit Function
2492 FEnd
2493 '-----------------------------------------------
2494 '
2495 'ねじ締め機通信確認
2496 '
2497 '-----------------------------------------------
2498 Function M% fScewTcomChk
2499     fScewTcomChk = 0
2500     '通信確認送信
2501     M_Out(MOUT_ScwT_ComChk%) = MOn%
2502     '通信確認受信待機
2503     Wait M_In(MIN_ScwT_comOK%) = MOn%
2504     '通信確認送信終了
2505     M_Out(MOUT_ScwT_ComChk%) = MOff%
2506     Exit Function
2507 FEnd
2508 '
2509 '
2510 '-----------------------------------------------
2511 '
2512 'ねじ締め開始送信
2513 '
2514 '-----------------------------------------------
2515 Function M% fScewTStart
2516     fScewTStart = 0
2517     'ねじ締め開始待機を受信
2518     Wait M_In(MIN_ScwT_STRec%) = MOn%
2519     Dly 0.1
2520     'ねじ締め開始受信を送信
2521     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
2522     Exit Function
2523 FEnd
2524 '
2525 '
2526 '-----------------------------------------------
2527 '
2528 'ねじ締め完了受信
2529 '
2530 '-----------------------------------------------
2531 Function M% fScewTFinish
2532     fScewTFinish = 0
2533     'ねじ締め完了待機を受信
2534     Wait M_In(MIN_ScwT_Fin%) = MOn%
2535     Dly 0.1
2536     'ねじ締め完了受信を送信
2537     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
2538     Exit Function
2539 FEnd
2540 '
2541 '
2542 '-----------------------------------------------
2543 '
2544 '条件xx停止受信
2545 '
2546 '-----------------------------------------------
2547 Function M% fScewTCaseStop(ByVal MCase%())
2548     fScewTCaseStop = 0
2549     '条件xx停止を受信
2550     Wait M_In(MCase%(1)) = MOn%
2551     Dly 0.1
2552     '条件xx停止受信を送信
2553     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
2554     Exit Function
2555 FEnd
2556 '
2557 '-----------------------------------------------
2558 '
2559 '再開始受信
2560 '
2561 '-----------------------------------------------
2562 Function M% fScewTReStart()
2563     fScewTReStart = 0
2564     '再開始を受信
2565     Wait M_In(MIN_ScwT_ReST%) = MOn%
2566     Dly 0.1
2567     '再開始受信を送信
2568     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
2569     Exit Function
2570 FEnd
2571 '
2572 '■fErrorProcess
2573 '<summary>
2574 'エラー処理
2575 '</summary>
2576 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
2577 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
2578 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
2579 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
2580 '<make>
2581 '2021/11/5 中村天哉
2582 '</make>
2583 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2584     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
2585     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
2586     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
2587     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
2588 *RETRY_ERR_PROCESS
2589      M_20# = MClear%     '初期化
2590 '        'エラー処理記述
2591         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2592 '        'GOT KEY入力待ち
2593         MKeyNumber = fnKEY_WAIT()
2594 '        '
2595         If MKeyNumber = MAbout% Then   '停止を選択した場合
2596             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2597 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2598             Break
2599          '
2600         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2601             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2602 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2603         '
2604         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2605             M_20# = MNext%            'M_20# プログラム間共通外部変数
2606 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2607          '
2608         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2609             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2610 '            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2611             Break
2612         '
2613         EndIf
2614         '
2615         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2616         fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2617     Exit Function
2618 FEnd
2619 '
2620 '■fnInitialZone
2621 ''' <summary>
2622 ''' 現在位置から上空に待避し、初期位置に戻る
2623 ''' </summary>
2624 ''' <param name="posNum%">移動先のポジション番号</param>
2625 ''' <remarks>
2626 ''' Date : 2021/12/2 : M.Hayakawa
2627 ''' Update:2022/06/2 : M.Hayakawa 他工程の非常停止復帰に合わせて変更
2628 ''' </remarks>
2629 Function fnInitialZone()
2630     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中]
2631 '
2632     Ovrd 5
2633 ' 上空退避
2634     PActive = P_Curr
2635     Pmove = PActive
2636 '
2637     If PActive.X > 580 Then
2638         Pmove.Z =380        'パレット上に腕を伸ばしているときは500まで上げられない為、例外処置
2639     Else
2640         Pmove.Z =500        '上記以外はZ:500まで持ち上げ
2641     EndIf
2642 '
2643     Mvs Pmove
2644     Mov PInitialPosition
2645 ' ロックを開放
2646     InitialState()
2647 ' 一旦停止
2648     fErrorProcess(20,70,256,0)
2649     Exit Function
2650  FEnd
2651 '
2652 '■InitialState
2653 ''' <summary>
2654 ''' ハンド、治具を初期位置にする
2655 ''' </summary>
2656 ''' <returns>   0 : OK
2657 '''             1 : NG
2658 ''' </returns>
2659 ''' <remarks>
2660 ''' Date : 2021/12/2 : M.Hayakawa
2661 ''' </remarks>
2662 Function M% InitialState()
2663     InitialState = 0
2664     '
2665     '位置決め解除
2666     M_Out(12264) = 0
2667     M_Out(12265)=1 Dly 0.3                  'プッシュ解除
2668     'Wait M_In(11276)=1                      'プッシュ戻端検出(修正につきコメントアウト(8/26中村))
2669     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    'プッシュ位置戻端検出(8/26中村)
2670     If MRtn = 0 Then
2671         fErrorProcess(11,234,284,0)
2672         Select M_20#
2673             Case MAbout%                    '停止が押された場合
2674                 InitialState = 1
2675                 Break
2676             Case MNgProcess%
2677                 InitialState = 1
2678                 Break
2679             Case MContinue%                 'リトライが押された場合
2680                 M_20# = MClear%
2681                 InitialState = 0
2682                 Break
2683             Case MNext%                     '次へが押された場合
2684                 M_20# = MClear%
2685                 InitialState = 0
2686                 Break
2687         End Select
2688     EndIf
2689     *RETRY_POSITIONING_RESTORE
2690     '
2691     M_Out(12262) = 0
2692     M_Out(12263)=1 Dly 0.3                  '位置決め解除
2693     'Wait M_In(11274)=1                      '位置決め戻端検出(修正につきコメントアウト(8/26中村))
2694     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '位置決め戻端検出(8/26中村)
2695     If MRtn = 0 Then
2696         fErrorProcess(11,234,284,0)
2697         Select M_20#
2698             Case MAbout%                    '停止が押された場合
2699                 InitialState = 1
2700                 Break
2701             Case MNgProcess%
2702                 InitialState = 1
2703                 Break
2704             Case MContinue%                 'リトライが押された場合
2705                 M_20# = MClear%
2706                 InitialState = 0
2707                 Break
2708             Case MNext%                     '次へが押された場合
2709                 M_20# = MClear%
2710                 InitialState = 0
2711                 Break
2712         End Select
2713     EndIf
2714     Exit Function
2715 FEnd
2716 '
2717 '■fnTorqueCheck
2718 ''' <summary>
2719 ''' トルクチェック動作用のメイン
2720 ''' </summary>
2721 ''' <remarks>
2722 ''' Date   : 2021/12/21 : H.AJI
2723 ''' </remarks>'
2724 Function M% fnTorqueCheck
2725     'トルクチェック中送信  搬送系停止
2726     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
2727     '
2728     fnTorqueCheck = 0
2729     Ovrd 20
2730     Mov PInitialPosition              '初期位置移動
2731     Ovrd 100
2732     '下記キー待ちの継続に反応させないため
2733     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
2734     Dly 0.2
2735     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
2736     '
2737     'M6340  トルクチェック受信
2738     'Dly 5.0
2739     M_Out(12340) = 1          'トルクチェック受信 M6340
2740     Dly 1.0
2741     M_Out(12340) = 0
2742     '
2743     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
2744     '
2745     MLoopFlg = 1
2746     While MLoopFlg = 1
2747         '
2748         Mov PInitialPosition              '初期位置移動
2749         '
2750         MKeyNumber = fnKEY_WAIT()
2751         Select MKeyNumber
2752             Case Is = 1           '停止
2753                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
2754                 Dly 1.0
2755                 M_Out(12343) = 0
2756                 Ovrd 20
2757                 Mov PTicketRead_1
2758                 Ovrd 100
2759                 M_20# = 1
2760                 MLoopFlg = -1
2761                 Break
2762             Case Is = 2           '次へ
2763                 Break
2764             Case Is = 3           '継続
2765                 Break
2766             Case Is = 4           'トルクチェック開始
2767                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
2768                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
2769                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
2770                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
2771                 MRet = fnMoveTorquePosi()
2772                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
2773                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2774                 Break
2775             Default
2776                 Break
2777         End Select
2778     WEnd
2779     '
2780     'トルクチェック中停止送信
2781     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
2782     '
2783     'ロボットの位置を元に戻す
2784     '
2785     Exit Function
2786  FEnd
2787  '
2788 '
2789 '
2790 '---------------------------
2791 '
2792 '    メイン画面の表示、非表示設定
2793 '         コメントD1001, D1002, D1003の設定
2794 '           MWindReSet = 0     画面非表示
2795 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
2796 '           MWindErrScr = 10    エラー画面 D1001, D1002
2797 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2798 '
2799 '---------------------------
2800 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2801     fnMainScreenOpen = 0
2802     '
2803    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2804         M_Out16(12480) = MCommentD1001            'D1001 コメント
2805     EndIf
2806     '
2807     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2808         M_Out16(12496) = MCommentD1002            'D1002 コメント
2809     EndIf
2810     '
2811     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2812         M_Out16(12512) = MCommentD1003            'D1003 コメント
2813     EndIf
2814     '
2815     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2816     M_Out(12362) = 1                         'ウィンド画面設定  M6362
2817     Dly 0.5
2818     M_Out(12362) = 0                         'ウィンド画面設定
2819     Exit Function
2820 FEnd
2821 '
2822 '■Main
2823 ''' <summary>
2824 ''' トルクチェック実動作
2825 ''' </summary>
2826 ''' <remarks>
2827 ''' Date   : 2021/12/21 : H.AJI
2828 ''' </remarks>'
2829 Function M% fnMoveTorquePosi
2830      fnMoveTorquePosi = 0
2831      Ovrd 50
2832      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
2833     '
2834     Spd M_NSpd
2835 '-------------      ドライバーRST
2836     M_Out(12240)=0     'ドライバーOFF CCW
2837     M_Out(12241)=0     'ドライバーOFF CW
2838     M_Out(12242)=1     'ドライバー解除 C1
2839     M_Out(12243)=1     'ドライバー解除 C2
2840     M_Out(12245)=0     'プログラム解除 F1/プログラム2
2841 '---------------------------------------
2842 '[P-11]
2843 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
2844     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
2845     Dly 0.1
2846 '-----------------------
2847    'Cnt 0                           'Cnt動作-2　終了
2848 '-----------------------
2849     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
2850     Dly 0.2
2851 '-----------------------
2852     ProgramBankSet(1,3)
2853     M_Out(12241)=0                   'ドライバーOFF  CW
2854     'Dly 0.1
2855 '--------------------------------
2856     Ovrd 40
2857    'Dly 0.1
2858 '--------------------------------  ネジ締め速度設定
2859     Spd 14                            'ライド 100-40 100% :Spd 12
2860     Dly 0.1
2861 '--------------------------------
2862 '--------------------------------
2863 '---------------------------------【ねじ締め動作】
2864 '
2865     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
2866    Mvs PTorqueCheck               'トルクチェック位置へ移動
2867     Dly 0.3                          '動作安定待ち
2868    M_Out(12241)=1                   'ドライバーON  CW
2869 '
2870     Wait M_In(11584)=1                '完了/エラー検出
2871     Dly 0.1
2872     Spd M_NSpd
2873    'Ovrd 20
2874     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
2875     Wait M_In(11257)=1                'ネジ完了SC
2876 '---------------------------------
2877     Dly 0.1
2878     M_Out(12241)=0                    'ドライバーOFF CW
2879     Dly 0.1
2880     M_Out(12242)=0                    'ドライバー解除 C1
2881     Dly 0.1
2882     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
2883     Dly 0.1
2884     M_Out(12245)=0                    'プログラム2解除 F1
2885 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
2886 '
2887     Mvs PTorqueCheck,-60                       'あえてmov から変更
2888     Dly 0.1
2889 '--------------------------------------------------------------
2890    'Ovrd 80
2891 '--------------------------------------------------------------
2892 '---------------------------------------
2893 '---------------------------------------
2894 '---------------------------------------エラー離脱処理
2895    *LBL1
2896    Fsc Off            '力覚センサ　Off   *STEP1は不要
2897    Mvs ,-100
2898    M_Out(12241)=0     'ドライバーOFF CW
2899    Dly 0.1
2900    M_Out(12242)=0     'ドライバー解除 C1
2901    Dly 0.1
2902    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
2903    Dly 0.1
2904    M_Out(12245)=0     'プログラム解除 F1
2905 '---------------------------------------
2906 '---------------------------------------
2907 '-------------
2908    'Mov PInitPos19049
2909    Dly 0.1
2910 '
2911 '
2912     Exit Function
2913 FEnd
2914 '
2915 '■Main
2916 ''' <summary>
2917 ''' 組立動作用のメイン
2918 ''' </summary>
2919 ''' <remarks>
2920 ''' Date   : 2021/07/07 : M.Hayakawa
2921 ''' </remarks>'
2922 Function Main
2923     MopeNo = M_21#         '外部変数にて動作番号代入
2924     '
2925     If M_Svo=0 Then
2926         Servo On
2927     EndIf
2928     Wait M_Svo=1
2929 '組立スタート日付時刻要求パルスON
2930     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
2931 'パトライト操作
2932     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
2933     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
2934     '
2935     M_20# = 0                                   'KEY入力初期化
2936     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
2937     MRet% = 0
2938 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
2939     PActive = P_Curr                    '現在位置を取得
2940     MRecoveryPass% = 0
2941     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
2942         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
2943             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
2944             MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
2945         EndIf
2946     EndIf
2947     EndIf
2948     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
2949         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
2950             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
2951                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
2952             EndIf
2953         EndIf
2954     EndIf
2955     If MRecoveryPass% = 0 Then
2956         fnInitialZone()        '復帰動作パスフラグが立っていない時は復帰動作を実行
2957     EndIf
2958     '
2959     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
2960         M_Out(12364) = 1            'toPLC_データ保存ON
2961 'トルクチェック
2962         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2963             MRet% = fnTorqueCheck()
2964             Break
2965         Else
2966 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
2967 '                MRtn = InspInit()               '画像処理初期化処理
2968 '            EndIf
2969             '
2970            M_20# = MClear%                    '初期化
2971 '組立開始
2972             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2973                 MRet% = fnAssyStart()
2974             Else
2975                 M_20# = MPass%
2976             EndIf
2977 '組立終了日付時刻
2978             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
2979             Wait M_In(11572) = 1            '日付取得完了
2980             Dly 0.1
2981             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
2982 'リフターユニットへのOUT
2983             '  KEY入力が何もない場合 OKと判断
2984             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
2985             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
2986 'OK/NGフラグ出力
2987             If M_20# <= 0 Then
2988                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
2989             ElseIf M_20# = MPass% Then
2990                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
2991             EndIf
2992 'PIASに組立完了書込み
2993             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
2994                 If M_20# = MPass% Then
2995                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
2996                 Else
2997                     'KEY入力がNGの場合
2998                     If M_20# = MNgProcess% Then
2999                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3000                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3001                         MRet% = fnPiasWrite(MNG%)
3002                        nAssyNgQty = nAssyNgQty + 1
3003                     EndIf
3004                     '
3005                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/07中村)
3006                     If M_20# = MAssyOK% Then
3007                             '-----------------------
3008                             'D732 -> D2600 コピー要求
3009                             M_Out(12566) = 1
3010 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3011                             M_Out(12566) = 0
3012                             '
3013                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3014                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3015                             '基板番号照合(PPは未使用）
3016 '                            MRet% = fnPCBNumberCheck()
3017                         Else
3018                             MRet% = 1
3019                         EndIf
3020                         '
3021                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3022                             If M_20# <> MAbout% Then
3023                                 '工程履歴OK書き込み
3024                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3025                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3026                                 MRet% = fnPiasWrite(MOK%)
3027                                 nAssyOkQty = 0
3028                                 nAssyOkQty = nAssyOkQty + 1
3029                             Else
3030                                 nAssyOkQty = nAssyOkQty + 1
3031                             EndIf
3032                         EndIf
3033                     EndIf
3034 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3035 '                    MRet% = fnPiasWrite(MOK%)
3036                 EndIf
3037             Else
3038                 nAssyOkQty = nAssyOkQty + 1
3039             EndIf
3040             '
3041             '組立終了日付時刻解除
3042             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3043             '投入数、組立OK数、組立NG数書込み
3044 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3045             '
3046 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3047 '                '画像処理終了処理
3048 '                MRtn = InspQuit()
3049 '            EndIf
3050         EndIf
3051         M_Out(12364) = 0                          'toPLC_データ保存OFF
3052     EndIf
3053 'パトライト操作
3054     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3055     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3056 'GOT表示
3057     fnAutoScreenComment(93)  'AUTO画面 工程完了
3058 FEnd
3059 End
3060 '
3061 'おまじないコメント
3062 '絶対削除するな
3063 '
3064 '
3065 '
3066 '
3067 '
PActive=(603.000,-149.180,450.000,-179.990,0.000,90.000,0.000,0.000)(7,0)
PInitialPosition=(250.000,0.000,450.000,180.000,0.000,180.000)(7,0)
Pmove=(603.000,-149.180,380.000,-179.990,0.000,90.000,0.000,0.000)(7,0)
PScrewSoc1=(300.800,-66.740,330.150,-180.000,0.000,90.000)(7,0)
PScrewSoc1_0=(300.800,-66.740,336.150,-180.000,0.000,90.000)(7,0)
PScrewSoc1_1=(300.800,-66.740,395.000,-180.000,0.000,90.000)(7,0)
PScrewSoc2=(322.310,-26.380,330.390,-180.000,0.000,90.000)(7,0)
PScrewSoc2_0=(322.310,-26.380,336.390,-180.000,0.000,90.000)(7,0)
PScrewSoc2_1=(322.310,-26.380,395.000,-180.000,0.000,90.000)(7,0)
PScrewSoc3=(382.420,-26.610,330.490,-180.000,0.000,90.000)(7,0)
PScrewSoc3_0=(382.420,-26.610,336.490,-180.000,0.000,90.000)(7,0)
PScrewSoc3_1=(382.420,-26.610,395.000,-180.000,0.000,90.000)(7,0)
PScrewSoc4=(392.510,-87.710,329.900,-180.000,0.000,90.000)(7,0)
PScrewSoc4_0=(392.510,-87.710,335.900,-180.000,0.000,90.000)(7,0)
PScrewSoc4_1=(392.510,-87.710,395.000,-180.000,0.000,90.000)(7,0)
PScrewSoc5=(371.800,-155.860,330.000,-180.000,0.000,90.000)(7,0)
PScrewSoc5_0=(371.800,-155.860,336.000,-180.000,0.000,90.000)(7,0)
PScrewSoc5_1=(371.800,-155.860,395.000,-180.000,0.000,90.000)(7,0)
PScrewSoc6=(322.080,-175.000,330.250,-180.000,0.000,90.000)(7,0)
PScrewSoc6_0=(322.080,-175.000,336.250,-180.000,0.000,90.000)(7,0)
PScrewSoc6_1=(322.080,-175.000,395.000,-180.000,0.000,90.000)(7,0)
PScrewSupply=(180.800,240.770,339.800,-180.000,0.000,-120.000)(7,0)
PScrewSupply_1=(180.800,240.770,380.000,-180.000,0.000,-120.000)(7,0)
PScrewSupply_2=(182.970,239.670,400.000,180.000,0.000,180.000)(7,0)
PScrewSupply_9=(78.320,270.810,429.990,-180.000,0.000,-120.000)(7,0)
PSocCheck=(325.250,-60.870,444.000,180.000,-0.010,-180.000)(7,0)
PSocCheck_1=(325.250,-60.870,470.000,180.000,-0.010,-180.000)(7,0)
PSocGet=(627.930,104.720,312.870,179.690,0.000,-179.370)(7,0)
PSocGet_1=(627.930,104.720,340.000,179.690,0.000,-179.370)(7,0)
PSocGet_2=(628.220,107.240,380.000,-179.930,0.040,-178.100)(7,0)
PSocPcbRead=(343.720,-16.250,435.000,-180.000,0.000,-180.000)(7,0)
PSocPcbRead_1=(343.720,-16.250,480.000,-180.000,0.000,-180.000)(7,0)
PSocPress=(393.390,10.430,354.200,180.000,0.000,-180.000)(7,0)
PSocPress_1=(393.390,10.430,369.000,-180.000,0.000,180.000)(7,0)
PSocPress_2=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PSocSet=(486.680,-100.680,350.000,179.650,0.000,-179.180)(7,0)
PSocSet_1=(486.680,-100.680,361.910,179.650,0.000,-179.180)(7,0)
PSocSet_2=(486.680,-100.680,380.000,179.650,0.000,-179.180)(7,0)
PTemp=(603.000,-149.180,450.000,-179.990,0.000,90.000,0.000,0.000)(7,0)
PTicketRead=(603.000,-149.180,373.000,-179.990,0.000,90.000)(7,0)
PTicketRead_1=(603.000,-149.180,450.000,-179.990,0.000,90.000)(7,0)
PTorqueCheck=(144.460,-240.780,340.000,-179.990,-0.010,90.020)(7,0)
PTorqueCheck_1=(144.450,-240.800,360.000,-179.990,0.000,90.010)(7,0)
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
PGetScrewPos(1)=(180.800,240.770,380.000,-180.000,0.000,-120.000,0.000,0.000)(7,0)
PGetScrewPos(2)=(182.970,239.670,400.000,180.000,0.000,180.000,0.000,0.000)(7,0)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(78.320,270.810,429.990,-180.000,0.000,-120.000,0.000,0.000)(7,0)
PGetScrewPos(10)=(180.800,240.770,339.800,-180.000,0.000,-120.000,0.000,0.000)(7,0)
PInspPosition(1)=(343.720,-16.250,435.000,-180.000,0.000,-180.000,0.000,0.000)(7,0)
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
PScrewPos(1)=(322.080,-175.000,395.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PScrewPos(2)=(322.080,-175.000,336.250,-180.000,0.000,90.000,0.000,0.000)(7,0)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(322.080,-175.000,330.250,-180.000,0.000,90.000,0.000,0.000)(7,0)
