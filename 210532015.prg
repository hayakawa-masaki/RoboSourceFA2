1 ' ===================================
2 '
3 '  21053001 STEP5 Assy5プログラム
4 '
5 ' 作成者：自動化T
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 ' ===================================
9 '===== <Insight定数> =====
10 '===== <Insight変数定義> =====
11 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
12 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
13 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
14 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
15 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
16 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
17 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
18 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
19 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
20 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
21 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
22 '
23 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
26 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
27 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
28 '
29 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
30 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
33 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
34 '作業用変数
35 Def Inte MInspErrNum                '検査実行エラー番号
36 Def Inte MInspNGStepNum             '検査実行NGStep番号
37 Def Inte MRtn                       'Function戻り値取得用
38 Def Inte MRtn2                      'Function戻り値取得用
39 Def Inte MRet3                      'Function戻り値取得用
40 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
41 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
42 Def Inte MovrdA                     'ネジ締めOvrd 可変用
43 Def Float MSpdA                     'ネジ締めSpd　可変用
44 Def Pos PTemp                       'ネジ締め上空位置計算用
45 '===== <Insight変数設定> =====
46 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
47 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
48 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
49 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
50 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
51 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
52 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
53 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
54 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
55 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
58 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
59 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
60 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
61 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
62 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
63 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
64 '===== <電ドラ変数定義> =====
65 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
66 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
67 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
68 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
69 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
70 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
71 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
72 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
73 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
74 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
75 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
76 '11584   'toRBトルクドライバ-COMP_ERR送信
77 Y60_Driver=12240 '電ドラ半時計回り CCW
78 Y61_Driver=12241 '電ドラ時計回り CW
79 Y62_Driver=12242 'バンクセッティング BANK C1
80 Y63_Driver=12243 'バンクセッティング BANK C2
81 Y64_Driver=12244 'バンクセッティング BANK C3
82 Y65_Driver=12245 'プログラムセッティング PRG SET F1
83 Y66_Driver=12246 'プログラムセッティング PRG SET F2
84 Y67_Driver=12247 'プログラムセッティング PRG SET F3
85 X34_ScrewReady1=11259 'ねじっこ1　Read
86 '===== <電ドラ定数> =====
87 Dim PScrewPos(10)       'ネジ締め用Function引数変数
88 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <ロボット定数> =====
92 '===== <ロボット変数定義> =====
93 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 'ウィンド画面番号設定
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
114 MClear% = 0        'KEY_のクリア
115 MAbout% = 1        'KEY_停止
116 MNext% = 2         'KEY_次のステップへ移行
117 MContinue% = 3     'KEY_継続 再度同じ動作を行う
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '組立完了
123 MPass% = 7         '工程パス
124 MPiasNG% = 8       'Pias確認時履歴NG
125 '
126 '初期化用KEY番号   '
127 MRobotInit1% = 11  '初期位置用
128 MRobotInit2% = 12  '初期位置用
129 MRobotInit3% = 13  '初期位置用
130 MRobotInit4% = 14  '初期位置用
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
133 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
134 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
135 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
138 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
139 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
140 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
141 '
142 MOK% = 1               '各判定用
143 MNG% = 0               '各判定用
144 MTIMEOUT% = -1         '各判定用
145 MJudge% = 0            '判定情報格納用
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10秒設定
149 MSETTIMEOUT03& = 3000&                 '3秒設定
150 MSETTIMEOUT01& = 1000&                 '1秒設定
151 MSETTIMEOUT05& = 5000&                 '5秒設定
152 MSETTIMEOUT009& = 900&                 '0.9秒設定
153 MSETTIMEOUT008& = 800&                 '0.8秒設定
154 MSETTIMEOUT007& = 700&                 '0.7秒設定
155 MSETTIMEOUT006& = 600&                 '0.6秒設定
156 MSETTIMEOUT005& = 500&                 '0.5秒設定
157 MSETTIMEOUT004& = 400&                 '0.4秒設定
158 MSETTIMEOUT003& = 300&                 '0.3秒設定
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC通信OK
161 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
162 MIN_PIAS_ComNG% = 11553                'PC通信NG
163 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
164 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
165 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
169 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
171 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
174 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
175 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
176 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
177 '
178 MOUT_PiasAssyResultOK% = 12549    '組立OK
179 MOUT_PiasAssyResultNG% = 12550    '組立NG
180 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
181 '
182 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
183 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
184 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
185 '
186 MIN_Insight_Use% = 11369               '画像確認ON
187 MIN_TorqueCheck% = 11348               'トルクチェック
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
191 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
196 '
197 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
198 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
201 '
202 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
203 '
204 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '投入数 演算変数
211 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
212 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
213 Def Inte MSuctionErrQty     '吸着エラー数 2022/04/27 渡辺
214 Def Inte nAssyOkQty         '未使用
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO変数定義> =====
218 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
219 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
220 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
221 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
222 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
223 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
224 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
225 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
226 '
227 Def Inte Y6A_VV1            ' アーム先端　ネジ吸着バルブ
228 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
229 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
230 '
231 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
232 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
233 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
234 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
235 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
236 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
237 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
238 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
239 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
240 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
241 '
242 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
243 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
244 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
245 '
246 Def Inte MOUT_LED1          ' 画像処理用LED照明
247 '
248 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
249 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
250 '
251 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
252 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
253 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
254 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
255 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
256 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
261 '
262 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
263 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
264 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
265 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
266 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
267 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
268 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
269 Y6A_VV1%    =  12250    ' アーム先端　ネジ吸着バルブ
270 Y6B_VB1%    =  12251    'アーム先端　吸着破壊バルブ
271 MOUT_VB1%   =  12251    ' アーム先端　ネジ吸着破壊バルブ
272 '
273 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
274 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
275 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
276 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
277 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
278 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
279 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
280 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
281 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
282 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
283 '
284 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
285 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
286 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
287 '
288 MOUT_LED1%  =  12239    ' 画像処理用LED照明
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
293 '
294 '共通
295 Def Inte MTEST_KEY                      'デバックテスト用
296 Def Inte MOn                            '出力=1
297 Def Inte MOff                           '出力=0
298 '
299 'ねじ締め装置_出力アドレス
300 Def Inte MOUT_ScwT_ComChk               '通信確認
301 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
302 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
303 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
304 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
305 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
306 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
307 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
308 'ねじ締め装置_入力アドレス
309 Def Inte MIN_ScwT_comOK                 '通信確認返信
310 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
311 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
312 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
313 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
314 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
315 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
316 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
317 '
318 Dim MScwT_Case1%(2)               '条件1停止変数
319 Dim MScwT_Case2%(2)               '条件2停止変数
320 Dim MScwT_Case3%(2)               '条件3停止変数
321 Dim MScwT_Case4%(2)               '条件4停止変数
322 Dim MScwT_Case5%(2)               '条件5停止変数
323 '
324 '共通
325 MTEST_KEY% = 11359                       'デバッグ用テストKEY
326 MOn% = 1                                 '出力 = 1
327 MOff% = 0                                '出力 = 0
328 '
329 'ねじ締め機_アドレス設定
330 MOUT_ScwT_ComChk% = 12832               '通信確認送信
331 MOUT_ScwT_ST% = 12865                   'ねじ締め開始を送信
332 MOUT_ScwT_ReSTOK% = 12866               '再開始受信を送信
333 MOUT_ScwT_FinOK% = 12868                'ねじ締め完了受信を送信
334 MOUT_ScwT_Case1OK% = 12874              '条件1停止受信を送信
335 MOUT_ScwT_Case2OK% = 12875              '条件2停止受信を送信
336 MOUT_ScwT_Case3OK% = 12876              '条件3停止受信を送信
337 MOUT_ScwT_Case4OK% = 12877              '条件4停止受信を送信
338 MOUT_ScwT_Case5OK% = 12878              '条件5停止受信を送信
339 '
340 MIN_ScwT_comOK% = 11840                 'ねじ締め装置から返信
341 MIN_ScwT_STRec% = 11873                 'ねじ締め開始を受信
342 MIN_ScwT_ReST% = 11874                  '再開始を受信
343 MIN_ScwT_Fin% = 11876                   'ねじ締め完了を受信
344 MIN_ScwT_Case1% = 11882                 '条件1停止待機を受信
345 MIN_ScwT_Case2% = 11883                 '条件2停止待機を受信
346 MIN_ScwT_Case3% = 11884                 '条件3停止待機を受信
347 MIN_ScwT_Case4% = 11885                 '条件4停止待機を受信
348 MIN_ScwT_Case5% = 11886                 '条件5停止待機を受信
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
361 '設定 InitialZoneBで使用する変数
362 Def Pos PActive       '直交座標系 位置変数 現在位置
363 Def Pos Pmove         '直交座標系 位置変数 移動先
364 Def Jnt JActive       '関節座標系 位置変数 現在位置
365 Def Jnt Jmove         '関節座標系 位置変数 移動先
366 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
367 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
368 Def Inte MJ6          'J6軸の値を比較する為の変数
369 Def Inte MStandby              '待機位置確認フラグ
370 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
371 '★注意★初期位置を変更した時には、変更が必要！
372 '
373 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
374 Function M% fnAssyStart
375     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
376 '   BaseUnit6通信確認
377     *RE_COM_CHECK
378     MRtn = 1    '初期化
379     If M_In(11920) = 0 Then     'BaseUnit6が拾得中のフラグを立てていない
380         If M_In(11930) = 0 And M_In(11931) = 0 Then   '通信にて回転角不明
381             Dly 2.3                                   '回転待ち
382             If M_In(11930) = 0 And M_In(11931) = 0 Then  'もう一度確認
383                 MRtn = 0                              '通信にて異常
384                 Break
385             EndIf
386             Break
387         EndIf
388         Break
389     EndIf
390     If MRtn = 1 Then GoTo *BU6Com_OK    '通信OKならラベルジャンプ
391     fErrorProcess(11,298,287,0)         '0,284→298,287に変更6/3中村
392     If M_20# = MNext% Then M_20# = MClear%
393     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
394     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
395     If M_20# = MContinue% Then GoTo *RE_COM_CHECK
396     *BU6Com_OK
397 '
398 '
399 ' PIASチケット読込み工程抜け確認
400     M_20# = MClear%                       '初期化
401 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
402 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
403 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
404 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
405 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
406 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
407 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
408 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
409 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
410 '    EndIf
411 '    '
412 '    '座標移動
413 '    '
414 '    '条件xx停止
415 '    fScewTCaseStop(MScwT_Case5%)
416 '    '
417 '    'ベースユニットKEY
418 '    Wait M_In(MTEST_KEY%) = MOn%
419 '    '
420 '    '再開始
421 '    fScewTReStart()
422 '    '
423 '    '座標移動
424 '    '
425 '    'ねじ締め完了
426 '    Mret% = fScewTFinish()
427 ' ネジ締めテスト終了
428 ' PIASテスト -----------
429 '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
430 '    MRet% = fnPiasWrite(MNG%)
431  '   MRet% = fnPCBNumberCheck()
432 ' PIASテスト終了 -------
433 '
434     '組立開始(9/6追加(中村))
435     'プログラム原点
436     Ovrd 100
437     ' ハンド状態初期化(10/29追加M.H)(2/11修正(中村))
438     Cmp Off                     'コンプライアンスモード終了
439     ColChk On                   '衝突検知ON
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
460 'Dly 10                                  'デバッグ用(22/09/30中村)
461     ' ねじ締め機テスト用 ----------
462      Mret% = fScrewTcomChk()
463     If Mret% = -1 Then GoTo *ASSY_ERROR_END
464     ' ねじ締め機通信開始
465 '    fScrewTStart()           '処理位置変更2/27中村
466     'チケットIDを読む
467     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
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
482 '        Mov PTicketRead_1           'チケットID読み取り回避点
483 '        Cnt 0
484 '        Mvs PTicketRead             'ID読み位置
485 '    EndIf
486 '
487 ' 2022/04/12 安全方向へ条件変更 渡辺
488 ' PInitialPosition 在席 MStandby=2
489 ' PTicketRead_1 在席 MStandby=1
490 '
491     MStandby = 0    '待機位置フラグを初期化
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
507         Mov PTicketRead_1           'チケットID読み取り回避点
508         Cnt 0
509     EndIf
510     If MStandby <> 0 Then GoTo *PositionOK
511     fErrorProcess(11,230,281,0)            '初期位置にいない時はエラーにする
512     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
513     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
514     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
515     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
516     *PositionOK
517 '
518     Mvs PTicketRead             'ID読み位置
519 '
520     M_Out(12259) = 0            'DVDメカチャック開OfF
521     M_Out(12258) = 1            'DVDメカチャック閉ON
522 '
523     '
524     MRtn = 1        'MRtn初期化
525 *RE_TICKET_READ
526 '    MRtn = fnPiasCheck()               'ID読み取り
527 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
528 'MInspGroup%(1) = 1              '検査G番号
529 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
530 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
531     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
532     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
533     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
534 EndIf
535 If MRtn = 1 Then GoTo *CompRead
536 '
537     'エラー時製品位置決めを解除
538 *RE_ERR_REL_1
539 If M_20# = MContinue% Then M_20# = MRtn
540 M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
541 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
542 '
543 If MRtn = 1 Then GoTo *CompErrorRelease
544 MRtn = M_20#        'M_20#一時避難
545 M_20# = MClear%
546 fErrorProcess(11,234,284,0)     '位置決め戻端エラー
547 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
548 If M_20# = MNext% Then M_20# = MRtn
549 If M_20# = MNgProcess% Then M_20# = MAbout%
550 *CompErrorRelease
551 '
552 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
553 If M_20# = MNext% Then M_20# = MPass%
554 Mvs PTicketRead_1                         '22/04/07 追加 渡辺
555 GoTo *ASSY_ERROR_END
556 *CompRead
557     fScrewTStart()           '処理位置変更2/27中村)
558 '
559 '
560 ''    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置変更2/7中村)
561 '    MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
562 '    *RE_ERR_REL_2
563 '    If M_20# = MContinue% Then M_20# = MRtn2
564 '    If MRtn = 0 Then
565 '        MRtn2 = 1       'MRtn2初期化
566 '        M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
567 '        Mov PInitialPosition  '"イニシャルに戻る動き"
568 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
569 '        If MRtn2 = 0 Then
570 '            MRtn2 = M_20#                   '位置決め戻端エラーならM_20#内部を一時避難
571 '            M_20# = MClear%                 'M_20#初期化
572 '            fErrorProcess(11,234,284,0)     '位置決め戻端エラー
573 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#に避難した値を代入
574 '                '位置決めエラーが発生して工程を抜ける場合停止処理を行う
575 '            If M_20# = MNgProcess% Then M_20# = MAbout%
576 '            Break
577 '        EndIf
578 '        Break
579 '            EndIf
580 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
581 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
582     '
583     'パレットから製品を取る
584     '
585     *RE_POSITIONING
586     '
587     M_Out(12262) = 1 Dly 0.5 '本体位置決め出端ON
588 '    Wait M_In(11273) = 1     '本体位置決め出端検出
589     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '本体位置決め出端検出
590     If MRtn = 1 Then GoTo *CompPositioning
591     fErrorProcess(11,231,282,0)
592     If M_20# = MNext% Then M_20# = MClear%
593     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
594     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
595     If M_20# = MContinue% Then GoTo *RE_POSITIONING
596     *CompPositioning
597 '
598     Mov PProductOnPltGet_2      '本体受け取り上空回避点
599 '
600 ''    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置変更3/14中村)
601 '    MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
602 '    *RE_ERR_REL_2
603 '    If M_20# = MContinue% Then M_20# = MRtn2
604 '    If MRtn = 0 Then
605 '        MRtn2 = 1       'MRtn2初期化
606 '        M_Out(12263) = 1 Dly 0.5    '製品位置決めを解除
607 '        Mov PInitialPosition  '"イニシャルに戻る動き"
608 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
609 '        If MRtn2 = 0 Then
610 '            MRtn2 = M_20#                   '位置決め戻端エラーならM_20#内部を一時避難
611 '            M_20# = MClear%                 'M_20#初期化
612 '            fErrorProcess(11,234,284,0)     '位置決め戻端エラー
613 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#に避難した値を代入
614 '                '位置決めエラーが発生して工程を抜ける場合停止処理を行う
615 '            If M_20# = MNgProcess% Then M_20# = MAbout%
616 '            Break
617 '        EndIf
618 '        Break
619 '            EndIf
620 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
621 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
622 '
623 '    Mov PProductOnPltGet_1      '本体受け取り上空
624     '
625     *RE_PLT_GET_1
626     '
627     M_Out(12256) = 0            '本体チャック閉OFF
628     M_Out(12257) = 1            '本体チャック開ON
629     '
630 '    Wait M_In(11265) = 1        '本体チャック開検出
631     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
632     If MRtn = 1 Then GoTo *CompPltGet1
633     fErrorProcess(11,244,284,0)
634     If M_20# = MNext% Then M_20# = MClear%
635     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
636     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
637     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
638     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
639     *CompPltGet1
640     '
641     Mov PProductOnPltGet_1      '本体受け取り上空
642     '
643     Ovrd 25
644 '    Fine 0.05 , P
645     Mvs PProductOnPltGet        '本体受け取り位置
646     Dly 0.1
647     M_Out(12257) = 0            '本体チャック開OFF
648     M_Out(12256) = 1            '本体チャック閉ON
649 '    Fine 0 , P
650     '
651     M_Out(12263) = 1 Dly 0.5                    '本体位置決め戻端ON
652 '    Wait M_In(11274) = 1     '本体位置決め戻端検出
653     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '本体位置決め戻端検出
654     If MRtn = 1 Then GoTo *CompPltGet2
655     M_Out(12256) = 0                            '本体チャック閉OFF
656     M_Out(12257) = 1                            '本体チャック開ON
657     Dly 2.0
658     Mvs PProductOnPltGet_1
659     Mov PProductOnPltGet_2
660     fErrorProcess(11,234,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
663     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
664     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
665     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
666     Mov PProductOnPltGet_1
667     Mvs PProductOnPltGet
668     M_Out(12257) = 0                            '本体チャック開OFF
669     M_Out(12256) = 1                            '本体チャック閉ON
670     Dly 2.0
671     *CompPltGet2
672     '
673 '    Wait M_In(11264) = 1        '本体検出
674     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '本体検出
675     If MRtn = 1 Then GoTo *CompPltGet3
676     M_Out(12256) = 0            '本体チャック閉OFF
677     M_Out(12257) = 1            '本体チャック開ON
678     Dly 2.0
679     Mvs PProductOnPltGet_1
680     Mov PProductOnPltGet_2
681     fErrorProcess(11,252,284,0)
682     If M_20# = MNext% Then M_20# = MClear%
683     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
684     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
685     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
686     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
687     Mov PProductOnPltGet_1
688     Mvs PProductOnPltGet
689     M_Out(12257) = 0            '本体チャック開OFF
690     M_Out(12256) = 1            '本体チャック閉ON
691     Dly 2.0
692     *CompPltGet3
693     '
694 '    Wait M_In(11266) = 1        '本体チャック閉検出
695     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
696     If MRtn = 1 Then GoTo *CompPltGet4
697     M_Out(12256) = 0            '本体チャック閉OFF
698     M_Out(12257) = 1            '本体チャック開ON
699     Dly 2.0
700     Mvs PProductOnPltGet_1
701     Mov PProductOnPltGet_2
702     Dly 0.1
703     M_Out(12257) = 0            '本体チャック開OFF
704     M_Out(12256) = 1            '本体チャック閉ON
705     Dly 3.0
706     fErrorProcess(11,245,284,0)
707     If M_20# = MNext% Then M_20# = MClear%
708     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 追加 渡辺
709     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
710     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
711     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
712     M_Out(12256) = 0            '本体チャック閉OFF
713     M_Out(12257) = 1            '本体チャック開ON
714     Dly 2.0
715     Mov PProductOnPltGet_1
716     Mvs PProductOnPltGet
717     M_Out(12257) = 0            '本体チャック開OFF
718     M_Out(12256) = 1            '本体チャック閉ON
719     Dly 2.0
720     *CompPltGet4
721     '
722     Dly 0.1                     '念のためディレイ(0.2→0.1に変更221218中村)
723     Cnt 1 , 100 , 100           '範囲変更(10→100、221219中村)
724     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
725     Mvs PProductOnPltGet_1      '本体受け取り上空
726     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
727     Ovrd 100
728     Accel 50 , 50
729     Mov PProductOnPltGet_2      '本体受け取り上空回避点
730     '
731     '製品をねじロボ2に置く
732     Mov PProductOnRoboSet_3     '経路
733     Accel 100 , 100
734     Cnt 0
735 '    Wait M_In(11888) = 1        'ねじロボ2停止1受信(処理位置,処理変更3/1中村)
736     MRtn = fScrewTighenRoboCheck(11888)    '停止状態を受信する
737     *RE_ERR_REL_2
738     If MRtn = 0 Then
739         Cnt 0
740         Mov PProductOnPltSet_2
741         Mov PProductOnPltSet_1
742         Mvs PProductOnPltSet
743         M_Out(12256) = 0        '本体チャック閉OFF
744         M_Out(12257) = 1        '本体チャック開ON
745         Dly 2.0
746         Mvs PProductOnPltSet_1
747         Mvs PProductOnPltSet_2
748         Mov PInitialPosition
749     EndIf
750     If MRtn = 0 Then GoTo *ASSY_ERROR_END
751     '
752     *RE_ROBO_SET_1
753     '
754     M_Out(12259) = 0            'DVDメカチャック開OfF
755     M_Out(12258) = 1            'DVDメカチャック閉ON
756 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
757     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
758     If MRtn = 1 Then GoTo *CompRoboSet1
759     fErrorProcess(11,269,284,0)
760     If M_20# = MNext% Then M_20# = MClear%
761     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
762     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
763     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
764     *CompRoboSet1
765 '
766     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
767 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
768     Ovrd 25
769     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
770     Mvs PProductOnRoboSet       'ねじロボ製品置き位置
771     M_Out(12866) = 1 Dly 0.3    'ねじロボ2動作再開(停止1～停止2)
772 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
773     MScrewRoboNgFlg% = 0
774     MRtn = fScrewTighenRoboCheck(11889)    '停止状態を受信する
775     If MRtn = 0 Then
776         MScrewRoboNgFlg% = 1
777     EndIf
778 '
779     *RE_ROBO_SET_2
780 '
781     M_Out(12256) = 0            '本体チャック閉OFF
782     M_Out(12257) = 1            '本体チャック開ON
783 '    Wait M_In(11265)            '本体チャック開検出
784     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
785     If MRtn = 1 Then GoTo *CompRoboSet2
786     fErrorProcess(11,244,284,0)
787     If M_20# = MNext% Then M_20# = MClear%
788     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
789     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
790     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
791     *CompRoboSet2
792     '
793     Mvs PProductOnRoboSet_1     'ねじロボ製品置き上空
794     Mvs PProductOnRoboSet_2     'ねじロボ製品置き上空回避点
795 '    Mvs PProductOnRoboSet_4     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)
796     Ovrd 100
797     Cnt 1 , 10 , 10
798     Mov PProductOnRoboSet_3     '経路
799     '
800     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
801     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
802 '
803 '
804 '
805     '
806     'チルトスライダーを押す
807     Cnt 1 , 10
808     Mov PPushTilt_3             'チルトスライダー押し向き変え回避点
809     Cnt 0
810     Mov PPushTilt_2             'チルトスライダー押し回避点
811     Ovrd 30
812     Mvs PPushTilt_1             'チルトスライダー押し上空
813     Spd 1000
814     Ovrd 5
815     Mvs PPushTilt               'チルトスライダー押し
816     Spd M_NSpd
817     Ovrd 50
818     Cnt 1 , 1 , 1
819     Mvs PPushTilt_1             'チルトスライダー押し上空
820     Cnt 1 , 10 , 10
821     Ovrd 100
822     Mov PPushTilt_2             'チルトスライダー押し回避点
823     Cnt 1 , 100 , 100           'Cntを100mm近傍で追加(221219中村)
824     '
825     '背面板を取る(コンプライアンスモード実装11/8中村)
826     Mov PPlateBackGet_2         '背面板受け取り上空回避点
827 '    Cnt 1 , 10'暫定
828     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止2～停止3)
829 '    MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
830 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
831 '    Mov PPlateBackGet_1         '背面板受け取り上空'暫定
832     Cnt 0'暫定
833     Mov PPlateBackGet_1         '背面板受け取り上空
834     '
835     *RE_PLATE_GET
836     '
837     Fine 0.05 , P               'ファイン命令ON
838     Ovrd 25
839     Mvs PPlateBackGet           '背面板受け取り位置
840 '    Dly 0.2                     '一時コメントアウト
841     M_Out(12257) = 0            '本体チャック開OFF
842     M_Out(12256) = 1            '本体チャック閉ON
843     '
844 '    Wait M_In(11266) = 1        '本体チャック閉検出
845     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
846     If MRtn = 1 Then GoTo *CompPlateGet_1
847     M_Out(12256) = 0            '本体チャック閉OFF
848     M_Out(12257) = 1            '本体チャック開ON
849     Mvs PPlateBackGet_1
850     fErrorProcess(11,245,293,0)     '284→293に変更6/3中村
851     If M_20# = MNext% Then M_20# = MClear%
852     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
853     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
854     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
855     Mvs PPlateBackGet           '背面板受け取り位置
856     M_Out(12257) = 0            '本体チャック開OFF
857     M_Out(12256) = 1            '本体チャック閉ON
858     *CompPlateGet_1
859     Fine 0 , P                  'ファイン命令OFF
860     '
861     Ovrd 5
862     Accel 25 , 100
863     Dly 0.7                     'ディレイ時間調節中(把持力確保)
864 '    CmpG 0.7,0.7,,,,,,       'X,Y軸ゲインを0.7に変更
865 '    ColChk Off                  '衝突検知OFF
866 '    Cmp Pos , &B11          'X,Y軸コンプライアンスモード開始
867     Mov PPlateBackGet_1         '背面板受け取り上空
868     Cnt 1 , 10 , 10
869 '    Cmp Off                     'コンプライアンスモード終了
870 '    ColChk On                   '衝突検知ON
871     Ovrd 50
872     Mov PPlateBackGet_2         '背面板受け取り上空回避点
873     Ovrd 100
874     Accel 100 , 100
875     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '背面パネルチェック
876     If MRtn = 1 Then GoTo *CompPlateGet_2
877     Cnt 0
878     fErrorProcess(11,267,293,0)                     '284→293に変更6/2中村
879     If M_20# = MNext% Then M_20# = MClear%
880     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
881     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
882     If M_20# = MContinue% Then
883         Mov PPlateBackGet_1
884         Dly 0.3
885         M_Out(12256) = 0            '本体チャック閉OFF
886         M_Out(12257) = 1            '本体チャック開ON
887         Dly 2.0
888     EndIf
889     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
890     *CompPlateGet_2    '
891     '背面板を置く
892 '    Wait M_In(11889) = 1        'ねじロボ2停止2受信
893     ColChk Off
894     Cnt 1 , 100 , 100           '100mm近傍追加(221219中村)
895     Mov PPlateBackSet_13        '背面板置き上空
896     Cnt 1 , 10 , 10
897 '
898     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        'もう一度背面パネルチェック
899     If MRtn = 1 Then GoTo *CompPlateGet_3
900     fErrorProcess(11,267,293,0)                     '284→293に変更6/3中村
901     If M_20# = MNext% Then M_20# = MClear%
902     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
903     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
904     If M_20# = MContinue% Then
905         Mov PPlateBackGet_2
906         Mov PPlateBackGet_1
907         M_Out(12256) = 0            '本体チャック閉OFF
908         M_Out(12257) = 1            '本体チャック開ON
909         Dly 2.0
910     EndIf
911     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
912     *CompPlateGet_3
913 '
914 '    ' 部品供給要求送信
915     M_Out(12787) = 1
916 '
917     MRtn = fScrewTighenRoboCheck(11890)    '停止状態を受信する'12/20位置の変更(中村)
918     If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
919     If MRtn = 0 Then GoTo *ASSY_ERROR_END
920 '
921     Mov PPlateBackSet_12        '爪入れ前回避点
922     Cnt 0
923     Ovrd 25
924     Accel 25 , 25
925     Mvs PPlateBackSet_11        '爪入れ込み前
926     Mvs PPlateBackSet_10        '爪入れ込み1
927     Mvs PPlateBackSet_9         '爪入れ込み2
928 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
929 '    Cmp Pos, &B001000
930     Cnt 1           '0.1→0.2mm近傍に変更(221219中村)
931     Mov PPlateBackSet_8         '経路1
932     Mov PPlateBackSet_7         '経路2
933     Mov PPlateBackSet_6         '経路3
934     Mov PPlateBackSet_5         '経路4
935     Mov PPlateBackSet_4         '経路5
936     Mov PPlateBackSet_3         '経路6
937     Mov PPlateBackSet_2         '経路7
938     Mov PPlateBackSet_1         '経路8
939     Mov PPlateBackSet           '背面板離し位置
940 '    Cmp Off
941     Accel 100 , 100
942     Cnt 0
943     Dly 0.1
944     *RE_PLATE_SET
945     M_Out(12256) = 0            '本体チャック閉OFF
946     M_Out(12257) = 1            '本体チャック開ON
947     '
948 '    Wait M_In(11265)            '本体チャック開検出
949     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
950     If MRtn = 1 Then GoTo *CompPlateSet
951     fErrorProcess(11,244,284,0)
952     If M_20# = MNext% Then M_20# = MClear%
953     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
954     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
955     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
956     *CompPlateSet
957     '
958 '
959 '-----暫定押し-------------------------------------(22/12/14中村)ここから
960 *RE_BUCK_PUSH
961     M_20# = MClear%
962     Mov PPlateBackPush_2
963 '
964     M_Out(12257) = 0            '本体チャック開OFF
965     M_Out(12256) = 1            '本体チャック閉ON
966 '
967     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
968 '
969     If MRtn = 1 Then GoTo *CompBuckPushSetting  '正常なら押し動作へ
970 '
971     fErrorProcess(11,245,287,0) '閉端センサーNG時エラー表示
972         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
973         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
974         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NGが押されたらエラーエンドへ
975         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       'リトライが押されたらもう一度閉じる
976 '
977 *CompBuckPushSetting
978 '
979     Mvs PPlateBackPush_1
980     Ovrd 10
981     Mvs PPlateBackPush
982 '    Dly 0.1     'クランプをするので削除(221219中村)
983 '背面クランプここから(12/15)
984     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
985     MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
986         If MRtn = 0 Then
987             Mvs PPlateBackPush_1
988             Mov PPlateBackSet_12
989             Mov PInitialPosition    '"イニシャルに戻る動き"
990         EndIf
991         If MRtn = 0 Then GoTo *ASSY_ERROR_END
992 '背面クランプここまで(12/15)
993     Ovrd 50                     '20→50に変更(221219中村)
994     Mvs PPlateBackPush_1
995 *RE_CHUCK_OPEN
996     M_20# = MClear%
997     M_Out(12256) = 0            '本体チャック閉OFF
998     M_Out(12257) = 1            '本体チャック開ON
999     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1000     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
1001     fErrorProcess(11,244,284,0)
1002         If M_20# = MNext% Then M_20# = MClear%              '次へが押されたら値を初期化
1003         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '停止が押されたらエラーエンドへ
1004         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NGが押されたらエラーエンドへ
1005         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       'リトライが押されたらもう一度閉じる
1006 *CompChuckOpenForBackPush
1007 '-----暫定押し-------------------------------------(22/12/14中村)ここまで
1008 '
1009     ColChk On
1010     Mov PPlateBackSet_13        '背面板置き上空
1011 '    M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～停止4)
1012     Ovrd 100
1013     'ねじロボ製品クランプ固定待ち(コメントアウト221215中村)
1014 '    Wait M_In(11891) = 1        'ねじロボ2停止4受信
1015 'MRtn = fScrewTighenRoboCheck(11891)    '停止状態を受信する
1016 'If MRtn = 0 Then Mov PInitialPosition    '"イニシャルに戻る動き"
1017 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
1018     '
1019     'ねじロボ引き込み待ち
1020     M_Out(12866) = 1 Dly 0.5    'ねじロボ2動作再開(停止3～完了)
1021     '
1022     'DVDメカを取る
1023     M_Out(12258) = 0            'DVDメカチャック閉OFF
1024     M_Out(12259) = 1            'DVDメカチャック開ON
1025     '
1026     Mov PMechaGet_3             '経路1
1027     Mov PMechaGet_2             'DVDメカ受け取り上空回避点
1028 '    Wait M_In(11272)            '部品供給機Ready
1029 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '部品供給機Ready
1030 '    If MRtn = 0 Then
1031 '        fErrorProcess()         'エラー処理
1032 '    EndIf
1033 '
1034 '    ' 部品供給要求送信(処理位置変更)
1035     fnAutoScreenComment(513)    '状態表示[部品供給待ち] 2022/04/26 渡辺
1036 '    M_Out(12787) = 1
1037     '    ' 部品供給完了待ち(処理変更2/27中村)
1038 *RE_FEEDER_READY
1039 '    Wait M_In(11810) = 1
1040 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '供給待ち
1041 If MRtn = 1 Then GoTo *CompFeederReady
1042 '   ' 部品供給要求終了
1043 M_Out(12787) = 0
1044 fErrorProcess(11,289,290,0)                '284→290に変更6/3中村
1045 If M_20# = MNext% Then M_20# = MClear%
1046 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1047     Mov PMechaGet_2
1048     Mov PMechaGet_3
1049     Mov PMechaGet_4
1050     Mov PInitialPosition
1051 EndIf
1052 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1053 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1054     ' 部品供給要求
1055 M_Out(12787) = 1
1056 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1057 *CompFeederReady
1058 '    ' 部品供給要求終了
1059     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1060     M_Out(12787) = 0
1061 '
1062     Mov PMechaGet_1             'DVDメカ受け取り上空
1063     '
1064     *RE_MECHA_GET_1
1065     '
1066     M_Out(12258) = 0            'DVDメカチャック閉OFF
1067     M_Out(12259) = 1            'DVDメカチャック開ON
1068     '
1069 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1070     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
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
1086     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1087     M_Out(12260) = 1            'DVDメカシリンダー出ON
1088 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1089     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
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
1106     Mvs PMechaGet               'DVDメカ受け取り位置
1107     Dly 0.1
1108 '
1109     MRtn = 0
1110     MRtn2 = 0
1111     *RE_MECHA_GET_2
1112     If M_20# = MContinue% Then M_20# = MClear%
1113     M_Out(12259) = 0            'DVDメカチャック開OfF
1114     M_Out(12258) = 1            'DVDメカチャック閉ON
1115     '
1116 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1117     If MRtn = 1 Then Dly 1.0
1118     If MRtn = 1 Then GoTo *CompMechaGet3
1119     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
1120     If M_20# = MNext% Then GoTo *CompMechaGet3
1121     If MRtn = 1 Then GoTo *CompMechaGet3
1122     M_Out(12258) = 0            'DVDメカチャック閉OfF
1123     M_Out(12259) = 1            'DVDメカチャック開ON
1124     Dly 2.0
1125     Mvs PMechaGet_1
1126     Mov PMechaGet_2
1127     M_Out(12259) = 0            'DVDメカチャック開OfF
1128     M_Out(12258) = 1            'DVDメカチャック閉ON
1129     Mov PMechaGet_3
1130     Mov PMechaGet_4
1131     fErrorProcess(11,269,294,0) '284→294に変更6/3中村
1132     If M_20# = MNext% Then
1133         M_20# = MClear%
1134         MRtn = 1
1135     EndIf
1136     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1137     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1138     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1139     M_Out(12258) = 0            'DVDメカチャック閉OfF
1140     M_Out(12259) = 1            'DVDメカチャック開ON
1141     Mov PMechaGet_3
1142     Mov PMechaGet_2
1143     Mov PMechaGet_1
1144     Mvs PMechaGet
1145     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1146     *CompMechaGet3
1147     M_20# = MClear%
1148     '
1149 '    Wait M_In(11267) = 1        'DVDメカ検出
1150     If MRtn2 = 1 Then GoTo *CompMechaGet4
1151     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVDメカ検出
1152     If MRtn2 = 1 Then GoTo *CompMechaGet4
1153     M_Out(12258) = 0            'DVDメカチャック閉OfF
1154     M_Out(12259) = 1            'DVDメカチャック開ON
1155     Dly 2.0
1156     Mvs PMechaGet_1
1157     Mov PMechaGet_2
1158     M_Out(12259) = 0            'DVDメカチャック開OfF
1159     M_Out(12258) = 1            'DVDメカチャック閉ON
1160     Mov PMechaGet_3
1161     Mov PMechaGet_4
1162     fErrorProcess(11,273,294,0) '284→294に変更6/3中村
1163     If M_20# = MNext% Then
1164         M_20# = MClear%
1165         MRtn2 = 1
1166     EndIf
1167     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1168     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1169     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1170     M_Out(12258) = 0            'DVDメカチャック閉OfF
1171     M_Out(12259) = 1            'DVDメカチャック開ON
1172     Mov PMechaGet_3
1173     Mov PMechaGet_2
1174     Mov PMechaGet_1
1175     Mvs PMechaGet
1176     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1177     *CompMechaGet4
1178     M_20# = MClear%
1179     Dly 0.5
1180     '
1181     Mvs PMechaGet_1             'DVDメカ受け取り上空
1182 '    *RE_MECHA_GET_3
1183     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1184     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1185 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1186     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1187 '    If MRtn = 1 Then GoTo *CompMechaGet5       '処理位置変更2/11中村
1188 '    fErrorProcess(11,272,284,0)
1189 '    If M_20# = MNext% Then M_20# = MClear%
1190 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1191 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1192 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1193 '    *CompMechaGet5
1194     '
1195     If MRtn = 1 Then Ovrd 100
1196     Mov PMechaGet_2             'DVDメカ受け取り上空回避点
1197 '    ' 部品供給要求終了
1198     M_Out(12787) = 0
1199 '    ' 部品取得完了送信(パルス)
1200     M_Out(12800) = 1 Dly 0.5
1201     Mov PMechaGet_3             '経路1
1202     Mov PMechaGet_4             '経路2
1203 '
1204     *RE_MECHA_GET_3
1205     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1206     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1207     If MRtn = 1 Then GoTo *CompMechaGet5
1208     fErrorProcess(11,272,284,0)
1209     If M_20# = MNext% Then M_20# = MClear%
1210     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1211     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1212     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1213     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1214     *CompMechaGet5
1215     '
1216     'DVDメカを仮置き台へ置く
1217 '    Wait M_In(11920) = 0             'BaseUnit6側仮置き台フラグ確認(処理位置変更2/11中村)
1218 '
1219 '   仮置き台が回転中か確認
1220     *Loop_CW_CCW_1
1221     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1222     *Next_CW_CCW_1
1223     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1          'BaseUnit6側仮置き台フラグ確認
1224     *OK_FLG_1
1225 '
1226     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1227     '
1228     'DVDメカが仮置き台に置かれていないかの確認(追加ここから10/1中村)
1229     MRtn = 1
1230     If M_In(11931) = 1 Then          '回転方向の確認(CW方向)
1231         If M_In(11928) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1232             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1233             Wait M_In(11930) = 1     '仮置き台回転待ち
1234             MRtn = 0
1235         EndIf
1236     ElseIf M_In(11930) = 1 Then      '回転方向の確認(CCW方向)
1237         If M_In(11929) = 0 Then      'BaseUnit5側にDVDメカが置かれていた場合
1238             M_Out(12912) = 0         '仮置き台フラグ回収(回転可能状態にするため)
1239             Wait M_In(11931) = 1     '仮置き台回転待ち
1240             MRtn = 0
1241         EndIf
1242     Else
1243         MRtn = 0
1244     EndIf
1245     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1246     '
1247 *Loop_CW_CCW_S
1248     fnAutoScreenComment(530)    '状態表示[工程６の動作終了待ち] 2022/04/26 渡辺
1249 'Ver 0.4 追加 -----------------------
1250 '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1251     MRtn = 0
1252     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1253     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1254     If MRtn = 1 Then Dly 0.7
1255     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '工程6の動作中フラグがONしていたら再度ループ
1256     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1257 'Ver 0.4 ここまで -------------------
1258 '
1259     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
1260     Mov PMechaSet_3             'DVDメカ仮置き回避点1
1261 'Dly 5.0   'デバッグ用
1262 '
1263 *Loop_CW_CCW_2
1264 'Ver 0.4 追加 -----------------------
1265     '自工程が12912 = 1 を出力した後、再度 工程6側の動作中監視を行う
1266     MRtn = 0
1267     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec 工程6のフラグON監視
1268     If MRtn = 1 Then M_Out(12912) = 0                  '仮置き台フラグ回収 工程6優先のため12912=0を出力
1269     If MRtn = 1 Then Dly 0.7
1270     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '工程6の動作中フラグがONしていたら再度ループ
1271     M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1272 'Ver 0.4 ここまで -------------------
1273 '
1274     Mov PMechaSet_2             'DVDメカ仮置き回避点2
1275 '
1276 '    *Loop_CW_CCW_2  '仮置き台の状態をもう一度確認する(コメントアウト2/27中村)
1277 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1278 '    *Next_CW_CCW_2
1279 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2         'BaseUnit6側仮置き台フラグ確認
1280 '    *OK_FLG_2
1281 ''
1282 '    M_Out(12912) = 1                 '仮置き台フラグ立て(仮置き台の状態固定の為)
1283 '
1284     '
1285     *RE_MECHA_SET_1
1286     If M_20# = MContinue% Then M_20# = MClear%
1287     Ovrd 25
1288     M_Out(12261) = 0            'DVDメカシリンダー戻OFF
1289     M_Out(12260) = 1            'DVDメカシリンダー出ON
1290 '    Wait M_In(11271) = 1        'DVDメカシリンダー出検出
1291     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVDメカシリンダー出検出
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
1308 '    Wait M_In(11920) = 0        'BaseUnit6側仮置き台フラグ確認(コメントアウト2/27中村)
1309 '    M_Out(12912) = 1            '仮置き台フラグ立て
1310     If M_In(11931) = 1 Then     '回転方向の確認(CW方向)(追加ここまで10/1中村)
1311         Mov PMechaSet1_1        'DVDメカ仮置き上空1
1312         Ovrd 10
1313         Mvs PMechaSet1          'DVDメカ仮置き位置1
1314         Dly 0.1
1315         M_Out(12258) = 0        'DVDメカチャック閉OFF
1316         M_Out(12259) = 1        'DVDメカチャック開ON
1317 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1318         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1319         Mvs PMechaSet1_1        'DVDメカ仮置き上空1
1320     ElseIf M_In(11930) = 1 Then '回転方向の確認(CCW方向)(追加ここから10/1中村)
1321         Mov PMechaSet2_1        'DVDメカ仮置き上空
1322         Ovrd 10
1323         Mvs PMechaSet2          'DVDメカ仮置き位置2
1324         Dly 0.1
1325         M_Out(12258) = 0        'DVDメカチャック閉OFF
1326         M_Out(12259) = 1        'DVDメカチャック開ON
1327 '        Wait M_In(11268) = 1    'DVDメカチャック開検出
1328         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1329         Mvs PMechaSet2_1        'DVDメカ仮置き上空2
1330     'Else
1331         'エラー文(仮置き台が正常な位置に無い)
1332     EndIf                       '追加ここまで10/1中村
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
1345     M_Out(12260) = 0            'DVDメカシリンダー出OFF
1346     M_Out(12261) = 1            'DVDメカシリンダー戻ON
1347 '    Wait M_In(11270) = 1        'DVDメカシリンダー戻検出
1348     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVDメカシリンダー戻検出
1349     If MRtn = 1 Then GoTo *CompMechaSet3
1350     fErrorProcess(11,272,284,0)
1351     If M_20# = MNext% Then M_20# = MClear%
1352     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1353     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1354     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1355     *CompMechaSet3
1356     '
1357     Mov PMechaSet_2             'DVDメカ仮置き回避点2
1358     M_Out(12912) = 0                  '仮置き台フラグ回収(追加10/1中村)
1359     '
1360     'ねじロボ2の製品を取る
1361     Mov PProductOnRoboGet_4     '経路3から4へ
1362     M_Out(12259) = 0            'DVDメカチャック開OfF
1363     M_Out(12258) = 1            'DVDメカチャック閉ON
1364 '    Wait M_In(11876) = 1        'ねじロボ2完了待機を受信
1365 MRtn = fScrewTighenRoboCheck(11876)    '停止状態を受信する
1366 If MRtn = 0 Then Mov PInitialPosition   '"イニシャルに戻る動き"
1367 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1368 '
1369     *RE_ROBO_GET_1
1370 '
1371     M_Out(12259) = 0            'DVDメカチャック開OFF
1372     M_Out(12258) = 1            'DVDメカチャック閉ON
1373     If M_20# = MContinue% Then Dly 0.5
1374 '
1375 '    Wait M_In(11269) = 1        'DVDメカチャック閉検出
1376     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVDメカチャック閉検出
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
1387     Mov PProductOnRoboGet_3     'ねじロボ製品取り上空回避点2から3へ
1388 '    Ovrd 20
1389     Mvs PProductOnRoboGet_2     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から2へ
1390     Ovrd 20
1391     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1392     Ovrd 10
1393     Mvs PProductOnRoboGet       'ねじロボ製品取り位置
1394     Dly 0.1                     '0.2→0.1に変更(221218中村)
1395 '
1396     *RE_ROBO_GET_2
1397 '
1398     M_Out(12257) = 0            '本体チャック開OFF
1399     M_Out(12256) = 1            '本体チャック閉ON
1400 '
1401 '    Wait M_In(11266) = 1        '本体チャック閉検出
1402     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '本体チャック閉検出
1403     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1404     M_Out(12256) = 0            '本体チャック閉OFF
1405     M_Out(12257) = 1            '本体チャック開ON
1406     Dly 2.0
1407     Mvs PProductOnRoboGet_1
1408     Mvs PProductOnRoboGet_2
1409     Mov PProductOnRoboGet_3
1410     Mov PProductOnRoboGet_4
1411     Mov PInitialPosition
1412     M_Out(12257) = 0            '本体チャック開OFF
1413     M_Out(12256) = 1            '本体チャック閉ON
1414     Dly 1.0
1415     fErrorProcess(11,245,284,0)
1416     If M_20# = MNext% Then MRtn = 1
1417     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1418     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1419     M_Out(12256) = 0            '本体チャック閉OFF
1420     M_Out(12257) = 1            '本体チャック開ON
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
1431     Dly 0.1                     '0.2→0.1に変更(221218中村)
1432     Mvs PProductOnRoboGet_1     'ねじロボ製品取り上空
1433     Ovrd 50
1434     Mvs PProductOnRoboGet_2     'ねじロボ製品取り上空回避点(9/27暫定コメントアウト)12/15コメント解除
1435     Mvs PProductOnRoboGet_3     'ねじロボ製品置き上空(治具改造後従来の挙動では無理な場合)11/24追加(中村)4から3へ
1436     Ovrd 100
1437     Mov PProductOnRoboGet_4     '経路3から4へ
1438     Cnt 1 , 100 , 100           '10→100に変更(221219中村)
1439 '
1440     M_Out(12868) = 1 Dly 0.3    'ねじロボ2動作完了を送信
1441     *RE_ROBO_GET_3
1442     M_Out(12258) = 0            'DVDメカチャック閉OFF
1443     M_Out(12259) = 1            'DVDメカチャック開ON
1444 '    Wait M_In(11268) = 1        'DVDメカチャック開検出
1445     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVDメカチャック開検出
1446     If MRtn = 1 Then GoTo *CompRoboGet3
1447     fErrorProcess(11,270,284,0)
1448     If M_20# = MNext% Then M_20# = MClear%
1449     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1450     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1451     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1452     *CompRoboGet3
1453     '
1454     'パレットへ製品を置く
1455     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1456     Cnt 1 , 10
1457     Mov PProductOnPltSet_1      '本体置き位置上空
1458     Cnt 0
1459     Ovrd 10
1460     Mvs PProductOnPltSet        '本体置き位置
1461     Dly 0.1                     '0.5→0.1に変更(221219中村)
1462 '
1463     *RE_PLT_SET
1464 '
1465     M_Out(12256) = 0            '本体チャック閉OFF
1466     M_Out(12257) = 1            '本体チャック開ON
1467 '
1468     Wait M_In(11265) = 1        '本体チャック開検出
1469 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
1470     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
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
1484     Mvs PProductOnPltSet_1      '本体置き位置上空
1485     Ovrd 100
1486     Cnt 1 , 10 , 10             '追加(221219中村)
1487     Mov PProductOnPltSet_2      '本体置き位置上空回避点
1488 '    Mov PInitialPosition        'イニシャルポジション
1489     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
1490     Mov PTicketRead_1           'チケットID読み取り回避点
1491     Cnt 0                       '追加(221219中村)
1492     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
1493     '
1494     'チケットID書き込み
1495     M_20# = MAssyOK%
1496     *ASSY_ERROR_END
1497     *AssyEnd
1498     *fnAssyStart_FEndPosi
1499 FEnd
1500 '
1501 '■fnPiasCheck
1502 ''' <summary>
1503 ''' PIASチケット読込み
1504 ''' </summary>
1505 ''' <returns>   0 : NG
1506 '''             1 : OK(読込み完了)
1507 ''' </returns>
1508 ''' <remarks>
1509 ''' Date   : 2021/07/07 : M.Hayakawa
1510 ''' </remarks>'
1511 Function M% fnPiasCheck
1512     fnPiasCheck = 0
1513     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1514     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1515 '
1516 *RETRY_PIAS
1517     M_20# = MClear%
1518     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1519     '
1520     '【IDチケット読み込み】
1521     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1522     MInspGroup%(1) = 1              '検査G番号
1523     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1524 '
1525     'エラーの場合
1526     If MRtn <> 1 Then
1527         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1528         If MRtn <> 1 Then
1529             'D720 -> D1300 コピー要求
1530             M_Out(12565) = 1
1531             Dly 0.5
1532             M_Out(12565) = 0
1533             'エラー処理記述
1534             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1535             'GOT KEY入力待ち
1536             MKeyNumber = fnKEY_WAIT()
1537             '
1538             Select MKeyNumber
1539                 Case MNext%         '次へを選択した場合
1540                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1541                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1542                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1543                     Break
1544                 Case MAbout%        '停止を選択した場合
1545                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1546                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1547                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1548                     Break
1549                 Case MNgProcess%    'NGを選択した場合
1550                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1551                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1552                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1553                     Break
1554                 Case MContinue%     '継続を選択した場合
1555                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1556                     M_20# = MContinue%
1557                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1558                     Break
1559             End Select
1560         EndIf
1561     EndIf
1562 '----------D720 -> D1300 コピー要求----------
1563     M_Out(12565) = 1
1564     Dly 0.5
1565     M_Out(12565) = 0
1566 '----------通信確認をする----------
1567     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1568     MRtn = 0                ' 初期化
1569     M_20# = MClear%         ' 初期化
1570     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1571     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1572     If MRtn <> 1 Then
1573         If M_20# = MContinue% Then
1574             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1575         Else
1576             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1577         EndIf
1578     EndIf
1579 '----------工程抜け確認----------
1580     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1581     MRtn = 0                ' 初期化
1582     M_20# = MClear%         ' 初期化
1583     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1584     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1585     If MRtn <> 1 Then
1586         If M_20# = MContinue% Then
1587             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1588         Else
1589             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1590         EndIf
1591     EndIf
1592     '
1593     fnPiasCheck = 1
1594     *fnPiasCheck_End
1595 FEnd
1596 '
1597 '■fnPCComuCheck
1598 ''' <summary>
1599 ''' PC-PLC通信チェック
1600 ''' </summary>
1601 ''' <returns>   0 : NG
1602 '''             1 : OK(読込み完了)
1603 ''' </returns>
1604 ''' <remarks>
1605 ''' Date   : 2021/07/07 : M.Hayakawa
1606 ''' </remarks>'
1607 Function M% fnPCComuCheck
1608     fnPCComuCheck = 0
1609     MJudge% = 0                                  '初期化
1610     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1611     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1612     '
1613     For MStaNo = 0 To 5
1614         '
1615         If M_In(MIN_PIAS_ComOK%) = 1 Then
1616             'PC通信OK(M400)
1617             MJudge% = MOK%
1618             MStaNo = 5
1619             Break
1620         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1621             'toRBT_通信確認time out
1622             MJudge% = MNG%
1623             MCommentD1001 = 15
1624             MCommentD1002 = 21
1625             MStaNo = 5
1626             Break
1627         Else
1628             'toRBT_通信確認time out
1629             MJudge% = MNG%
1630             MCommentD1001 = 14
1631             MCommentD1002 = 21
1632             Break
1633         EndIf
1634     Next MStaNo
1635     '
1636     '上記で返信フラグを受信してからPC通信確認OFF
1637     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1638     '
1639     'エラー画面
1640     If MJudge% <> MOK% Then
1641         M_20# = MClear%     '初期化
1642         'エラー処理記述
1643         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1644         'GOT KEY入力待ち
1645         MKeyNumber = fnKEY_WAIT()
1646         '
1647         If MKeyNumber = MAbout% Then            '停止を選択した場合
1648             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1649             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1650             Break
1651         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1652             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1653             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1654             Break
1655         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1656             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1657             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1658             Break
1659         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1660             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1661             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1662             Break
1663         EndIf
1664     Else
1665         'OKの場合
1666         fnPCComuCheck = 1
1667     EndIf
1668 FEnd
1669 '
1670 '■fnProcessCheck
1671 ''' <summary>
1672 ''' 工程抜け確認
1673 ''' </summary>
1674 ''' <returns>    1：工程履歴OK     0：異常終了
1675 '''             -1：前工程履歴NG  -2：自工程履歴あり
1676 '''             -3：モデル仕向NG  -4：タイムアウト
1677 '''             -5：履歴処理エラー
1678 ''' </returns>
1679 ''' <remarks>
1680 ''' Date   : 2021/07/07 : M.Hayakawa
1681 ''' </remarks>'
1682 Function M% fnProcessCheck
1683     fnProcessCheck = 0
1684     MJudge% = MNG%      '一旦NGを初期化とする
1685 '----------工程抜け確認----------
1686     MCommentD1001 = 0   'コメント初期化
1687     For MStaNo = 0 To 5
1688         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1689         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1690         '
1691         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1692             MJudge% = MOK%
1693             fnAutoScreenComment(85)     ' AUTO画面
1694             MStaNo = 5
1695             Break
1696         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1697             MFlgLoop% = 0
1698             MJudge% = MNG%
1699             MCommentD1001 = 27
1700             MCommentD1002 = 22
1701             fnAutoScreenComment(94)     ' AUTO画面
1702             fnProcessCheck = -2         ' NGは-2を返す
1703             MStaNo = 5
1704             Break
1705         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1706            MJudge% = MNG%
1707             MCommentD1001 = 31
1708             MCommentD1002 = 22
1709             fnAutoScreenComment(83)     ' AUTO画面
1710             fnProcessCheck = -3         ' NGは-3を返す
1711             MStaNo = 5
1712             Break
1713         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1714             '履歴NGは直ぐに終了せず繰り返し確認を行う
1715             '前工程の書込みが終了していない可能性があるため
1716             MJudge% = MNG%
1717             MCommentD1001 = 32
1718             MCommentD1002 = 22
1719             fnAutoScreenComment(84)     ' AUTO画面
1720             fnProcessCheck = -1         ' NGは-1を返す
1721             Dly 1.0
1722             '工程抜け確認OFF
1723             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1724             Dly 1.0
1725            'MStaNo = 5
1726             Break
1727         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1728             MFlgLoop% = 0
1729             MJudge% = MNG%
1730             MCommentD1001 = 29
1731             MCommentD1002 = 22
1732             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1733             fnProcessCheck = -5         ' NGは-5を返す
1734             MStaNo = 5
1735             Break
1736         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1737             MJudge% = MNG%
1738             If MCommentD1001 = 32 Then
1739                 '何もしない
1740             Else
1741                 MCommentD1001 = 26
1742             EndIf
1743             MCommentD1002 = 22
1744             fnProcessCheck = -4         ' NGは-4を返す
1745             MStaNo = 5
1746             Break
1747         Else
1748             MJudge% = MNG%
1749             MCommentD1001 = 28
1750             MCommentD1002 = 22
1751         EndIf
1752     Next MStaNo
1753     '工程抜け確認OFF
1754     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1755     '通過履歴NG 工程抜けの場合
1756     If MJudge% = MPass% Then
1757         M_20# = MPass%
1758     EndIf
1759     '
1760     'エラー画面
1761     If MJudge% <> MOK% Then
1762         M_20# = MClear%     '初期化
1763         'エラー処理記述
1764         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1765         'GOT KEY入力待ち
1766         MKeyNumber = fnKEY_WAIT()
1767         '
1768         Select MKeyNumber
1769             Case MAbout%        '停止を選択した場合
1770                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
1771                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1772                 Break
1773             Case MNext%         '次へを選択した場合
1774                 M_20# = MPass%          'M_20# プログラム間共通外部変数
1775                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1776                 Break
1777             Case MContinue%     '継続を選択した場合
1778                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
1779                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1780                 Break
1781             Case MNgProcess%    'NGを選択した場合
1782                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
1783                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1784                 Break
1785         End Select
1786     Else
1787         fnProcessCheck = 1  ' OKは1を返す
1788     EndIf
1789 FEnd
1790 '
1791 '■fnPiasWrite
1792 ''' <summary>
1793 ''' Pias 組立結果書込み要求
1794 ''' </summary>
1795 '''<param name="MFlg%">
1796 '''                 MOK%(1) = 工程履歴にOKを書込む
1797 '''                 MNG%(0) = 工程履歴にNGを書込む
1798 '''</param>
1799 '''<returns></returns>
1800 ''' <remarks>
1801 ''' Date   : 2021/07/07 : M.Hayakawa
1802 ''' </remarks>'
1803 Function M% fnPiasWrite(ByVal MFlg%)
1804       fnPiasWrite = 0
1805 *RETRY_PIASWRITE
1806     '
1807     '組立OK(MOK%)の場合　M306 ON
1808    '組立NG(MNG%)の場合　M307 ON
1809     If MFlg% = MOK% Then
1810         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1811     Else
1812         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1813     EndIf
1814     Dly 0.1                  '念のため
1815     '
1816     'Piasへ書込み開始 M305 -> ON
1817     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1818     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
1819     '
1820     MJudge% = MNG%
1821     '
1822     For MStaNo = 0 To 5
1823         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
1824             MJudge% = MOK%
1825             'MRet = fnAutoScreenComment(85)  'AUTO画面
1826             MStaNo = 5
1827             Break
1828         '
1829         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
1830             MJudge% = MNG%
1831             'MRet = fnAutoScreenComment(85)  'AUTO画面
1832            MCommentD1001 = 34
1833            MCommentD1002 = 25
1834             MStaNo = 5
1835             Break
1836         '
1837         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
1838             MJudge% = MNG%
1839             'MRet = fnAutoScreenComment(85)  'AUTO画面
1840            MCommentD1001 = 35
1841            MCommentD1002 = 25
1842             MStaNo = 5
1843             Break
1844         '
1845         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
1846             MJudge% = MNG%
1847             'MRet = fnAutoScreenComment(85)  'AUTO画面
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
1862     'Piasへ書込み開始 M305 -> OfF
1863     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1864     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1865     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1866     '
1867     '
1868     '通過履歴NG 工程抜けの場合
1869     If MJudge% = MPass% Then
1870         M_20# = MPass%
1871     EndIf
1872     '
1873    M_20# = MClear%     '初期化
1874     '
1875     'エラー画面
1876     If MJudge% < MOK% Then
1877     '
1878 '残しておくが現状では使用しないラベル
1879 *RETRY_ERR_WRITE
1880         M_20# = MClear%     '初期化
1881         'エラー処理記述
1882         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1883         'GOT KEY入力待ち
1884         MKeyNumber = fnKEY_WAIT()
1885         '
1886         If MKeyNumber = MAbout% Then   '停止を選択した場合
1887             M_20# = MAbout%            'M_20# プログラム間共通外部変数
1888            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1889             Break
1890         '
1891         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
1892             M_20# = MContinue%            'M_20# プログラム間共通外部変数
1893             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1894         '
1895         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
1896             M_20# = MPass%            'M_20# プログラム間共通外部変数
1897             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1898         '
1899         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
1900             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
1901            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
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
1916 '■fnPCBNumberCheck
1917 ''' <summary>
1918 ''' Pias 基板番号照合要求
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
1930     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
1931     'Piasへ基板照合開始 M310 -> ON
1932     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1933     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
1934     '
1935     MJudge% = MNG%
1936     '
1937     For MStaNo = 0 To 5
1938         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
1939             MJudge% = MOK%
1940             fnAutoScreenComment(96)  'AUTO画面
1941             MStaNo = 5
1942             Break
1943         '
1944         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
1945             MJudge% = MNG%
1946             fnAutoScreenComment(97)  'AUTO画面
1947             MCommentD1001 = 37
1948             MCommentD1002 = 25
1949             MStaNo = 5
1950             Break
1951         '
1952         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
1953             MJudge% = MNG%
1954             fnAutoScreenComment(98)  'AUTO画面
1955             MCommentD1001 = 38
1956             MCommentD1002 = 25
1957             MStaNo = 5
1958             Break
1959         '
1960         ElseIf M_In(11580) = 1 Then                         'time out
1961             MJudge% = MNG%
1962             fnAutoScreenComment(99)  'AUTO画面
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
1977     'Piasへ基板照合開始 M310 -> OfF
1978     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1979     '
1980     '
1981     '通過履歴NG 工程抜けの場合
1982     If MJudge% = MPass% Then
1983         M_20# = MPass%
1984     EndIf
1985     '
1986    M_20# = MClear%     '初期化
1987     '
1988     'エラー画面
1989     If MJudge% < MOK% Then
1990     '
1991 '残しておくが現状では使用しないラベル
1992 *RETRY_ERR_PCBNUMBER
1993         M_20# = MClear%     '初期化
1994         'エラー処理記述
1995         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1996         'GOT KEY入力待ち
1997         MKeyNumber = fnKEY_WAIT()
1998         '
1999         If MKeyNumber = MAbout% Then   '停止を選択した場合
2000             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2001             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2002             Break
2003         '
2004         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2005             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2006             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2007         '
2008         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2009             M_20# = MPass%            'M_20# プログラム間共通外部変数
2010             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2011         '
2012         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2013             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2014             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
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
2026 '■ScrewTight_S2
2027 ''' <summary>
2028 ''' ねじ締めを行う
2029 ''' </summary>
2030 '''<param name="PScrewPos()">
2031 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
2032 '''             PScrewPos(2)    ：ねじ締め回避点
2033 '''             PScrewPos(10)   ：ねじ締め終了高さ
2034 '''</param>
2035 '''<returns>整数
2036 '''         0=異常終了、1=正常終了
2037 '''</returns>
2038 ''' <remarks>
2039 ''' Date   : 2021/07/07 : M.Hayakawa
2040 ''' </remarks>'
2041 Function M% ScrewTight_S2(ByVal PScrewPosition())   'ネジ締め個別設定
2042     ScrewTight_S2 = 0
2043     MOKNGFlg = 0
2044     Ovrd 100
2045     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2046     ' 暫定
2047     Ovrd 5
2048     Mvs PScrewPosition(10),-10    ' パレット上ねじ締めS①の上空へ移動
2049 '    Ovrd MOvrdA
2050     '暫定マスク
2051 '    M_Out(Y62_Driver)=1     ' バンクセッティング　C1
2052 '    Dly 0.1
2053 '    M_Out(Y61_Driver)=1     'ドライバーON　CW
2054 '    'Spd 8.3 '外部ライド100  内部ライド60   'ライド100-40　100%：Spd　15　'ねじ締め速度設定
2055 '    Spd MSpdA               'ネジ締め時Spd個別設定
2056     ' 暫定移動のみ
2057     Mvs PScrewPosition(10)
2058 '    '
2059 '    Dly 0.1
2060 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2061 '    Wait M_In(11584)=1          '完了/エラー検出
2062 '    Dly 0.1
2063 '    Spd M_NSpd
2064 '    '
2065 '    If M_In(X28_Driver)=1 Then  'ねじトータルエラー検出時
2066 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2067 '        Dly 0.1
2068 '        M_Out(Y62_Driver)=0     'バンクセッティング解除　C1
2069 '        Dly 0.1
2070 '        M_Out(Y63_Driver)=0     'バンクセッティング解除　C1
2071 '        Dly 0.1
2072 '        M_Out(Y65_Driver)=0     'プログラム解除　F1
2073 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2074 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2075 '        MOKNGFlg = -1
2076 '        ScrewTight_S2 = 0
2077 '    Else
2078 '        Wait M_In(X29_Driver)=1 ' 正常完了時
2079 '        Dly 0.1
2080 '        M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2081 '        Dly 0.1
2082 '        M_Out(Y62_Driver)=0     'バンクセッティング解除
2083 '        Dly 0.1
2084 '        M_Out(Y6A_VV1)=0        'ねじ吸着　OFF
2085 '        Dly 0.1
2086 '        Mvs PScrewPos(2),-80    'パレット上ねじ締めS①の上空へ移動
2087 '        ScrewTight_S2 = 1
2088 '    EndIf
2089 ' 暫定
2090     Ovrd 10
2091     Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2092     Ovrd 100
2093 FEnd
2094 '
2095 '■ScrewGet_S3
2096 ''' <summary>
2097 ''' ねじ供給機からねじを得る
2098 ''' </summary>
2099 '''<param name="%"></param>
2100 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2101 '''         PScrewPos(2)    ：ねじ供給器回避点
2102 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2103 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2104 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2105 '''<returns>整数
2106 '''         0=異常終了、1=正常終了、-1=MネジセンサーNG、-2=MネジセンサーON、-3=吸着エラー
2107 '''</returns>
2108 ''' <remarks>
2109 ''' Date   : 2021/07/07 : M.Hayakawa
2110 ''' </remarks>'
2111 Function M% ScrewGet_S3(ByVal PScrewPosition())
2112     ScrewGet_S3 = 0
2113     MMScrewJudge% = 0
2114     'ねじ供給器初期動作エラーチェック
2115 ' ↓暫定削除
2116 '    Wait M_In(X34_ScrewReady1)=1 'ねじ供給器SがReadyになるまで待つ　←　何秒か待ってReadyにならなければ抜けるプログラムが必要？
2117 '    Ovrd 100
2118 '    If M_In(X33_SS2)=0 Then  'Mねじ検出センサがOFF（故障）していた場合
2119 '        Ovrd 30
2120 '        Mvs,-80             'その場所から80mm上空へ移動
2121 '        Mov PInitPos19049   '19049初期位置へ移動
2122 '        M_Out(Y6A_VV1)=0    'ねじ吸着 Off
2123 '        'NGとしてここの関数から抜ける
2124 '        ScrewGet_S3 = -1
2125 '        MMScrewJudge% = 1
2126 '        MCommentD1001 = 61
2127 '    EndIf
2128 '    If ScrewGet_S3 = 0 Then
2129 '        'Sタイト用ねじ供給機にMねじが混入していないか監視
2130 '        MMScrewJudge% = 0 'MMScrewJudgeを初期化する
2131 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2132 '        If MRtn = 0 Then
2133 '            Ovrd 30
2134 '            Mvs,-80            'その場所から50mm上空へ移動
2135 '            Mov PInitPos19049  '19049初期位置へ移動
2136 '            MMScrewJudge% = 2
2137 '            MRtn = All_CLamp_Release()'全てのクランプ解除へ分岐
2138 '            MCnt% = 2   '2を設定
2139 '            MCommentD1001 = 62
2140 '        EndIf
2141 '        If MMScrewJudge% = 2 Then
2142 '            ScrewGet_S3 = -2
2143 '        EndIf
2144 '    EndIf
2145 '    'Mネジ判定がONの場合 NGとして関数を抜ける
2146 '    If MMScrewJudge% = 2 Then
2147 '        ScrewGet_S3 = -2
2148 '    EndIf
2149     'Sネジ用ねじ太郎のMネジ混入確認用ここまで
2150     Ovrd 100
2151     Spd M_NSpd
2152     If MMScrewJudge% = 0 Then
2153         ScrewGet_S3 = 0
2154         M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2155         MScrewCnt% = 0
2156         MFinCnt% = 2
2157 '        For MCnt% = 0 To MFinCnt%
2158             Mov PScrewPosition(2)        ' ねじ供給機回避点
2159             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2160             Ovrd 80
2161             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2162             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2163             Mvs PScrewPosition(10), 1.2
2164             M_Out(Y6A_VV1)=1        ' ねじ吸着　ON
2165             'ビット回転
2166             M_Out(Y60_Driver)=1
2167             Dly 0.2
2168             '
2169             Ovrd 100
2170             JOvrd M_NJovrd
2171             Spd M_NSpd
2172             'ネジ吸着確認位置移動
2173             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2174             Mvs PScrewPosition(10), -15  ' ネジ吸着確認位置
2175             'ビット回転停止
2176             'M_Out(Y60_Driver)=0
2177             '
2178             '1秒間ネジ吸着確認
2179 ' 以下暫定削除
2180 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2181 '            'MRtn = 0'強制エラー
2182 '            '吸着エラーの場合
2183 '            'ネジをねじ太郎に戻す
2184 '            If MRtn = 0 Then
2185 '                Ovrd 30
2186 '                'ビット回転停止
2187 '                M_Out(Y60_Driver)=0
2188 '                'ネジ供給機上空
2189 '                Mvs PScrewPos(1)
2190 '                '更に上空
2191 '                Mov PScrewPos(1), -75
2192 '                'ネジ捨て位置
2193 '                Mov PScrewFeedS021
2194 '                '吸着OFF
2195 '                M_Out(Y6A_VV1)=0 'ねじ吸着　OFF
2196 '                Dly 0.2
2197 '                '破壊ON
2198 '                M_Out(Y6B_VB1)=1 '真空破壊ON
2199 '                'ビット回転
2200 '                M_Out(Y61_Driver)=1
2201 '                Dly 0.5
2202 '                '
2203 '                Ovrd 100
2204 '                JOvrd M_NJovrd
2205 '                Spd M_NSpd
2206 '                'ドライバーを上下させねじを振り落とす
2207 '                Mov PScrewFeedS021, 10
2208 '                Mov PScrewFeedS021
2209 '                Dly 0.1
2210 '                Mov PScrewFeedS021, 10
2211 '                Mov PScrewFeedS021
2212 '                '
2213 '                'ネジ落ち待ち
2214 '                'ビット回転停止
2215 '                M_Out(Y61_Driver)=0
2216 '                Dly 0.1
2217 '                '破壊OFF
2218 '                M_Out(Y6B_VB1)=0 '真空破壊OFF
2219 '                '
2220 '                '
2221 '                'ねじ落ちたとして、移動更に上空
2222 '                Mov PScrewPos(1), -75
2223 '                Ovrd 100
2224 '                Spd M_NSpd
2225 '                'ネジ供給機上空
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
2239         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2240         M_Out(Y60_Driver)=0     ' ビット回転停止
2241         M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2242         Mvs PScrewPosition(10), -15  ' ねじピックアップ位置 -15mm
2243         'もう一度吸着確認
2244 ' 以下暫定削除
2245 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2246 '        If MRtn = 0 Then      '吸着エラーの場合
2247 '            MCommentD1001 = 94
2248 '            MCommentD1002 = 95
2249 '            ScrewGet_S3 = -3
2250 '        EndIf
2251 '        If MRtn = 1 Then      '吸着OKの場合
2252 '            ScrewGet_S3 = 1
2253 '        EndIf
2254 '        Break
2255     Else
2256         'Mネジ
2257         If MMScrewJudge% = 2 Then
2258             ScrewGet_S3 = -2
2259         EndIf
2260     EndIf
2261 FEnd
2262 '
2263 '■fnKEY_WAIT()
2264 ''' <summary>
2265 ''' GOTからのキー入力待ち
2266 ''' </summary>
2267 '''<returns>1：停止    2：次へ
2268 '''         3：継続    4：トルクチェック開始
2269 '''         5：NG
2270 '''         11：ロボット初期位置1    12：ロボット初期位置2
2271 '''         13：ロボット初期位置3    14：ロボット初期位置4
2272 '''</returns>
2273 ''' <remarks>
2274 ''' Date   : 2021/07/07 : M.Hayakawa
2275 ''' </remarks>'
2276 Function M% fnKEY_WAIT()
2277     fnKEY_WAIT = 0
2278     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2279     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2280     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2281     '下記キー待ちの継続に反応させないため
2282     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2283     Dly 0.2
2284     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2285     MLocalLoopFlg=1
2286     While MLocalLoopFlg=1
2287         If M_In(11345) = 1 Then         '停止   M5345
2288             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2289             fnKEY_WAIT = 1
2290             MLocalLoopFlg=-1
2291             Break
2292         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2293             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2294             fnKEY_WAIT = 2
2295             MLocalLoopFlg=-1
2296             Break
2297         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2298             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2299             fnKEY_WAIT = 3
2300             MLocalLoopFlg=-1
2301             Break
2302         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2303             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2304             fnKEY_WAIT = 4
2305             MLocalLoopFlg=-1
2306             Break
2307         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2308             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2309             fnKEY_WAIT = 5
2310             MLocalLoopFlg=-1
2311             Break
2312             '
2313         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2314             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2315             fnKEY_WAIT = MRobotInit1%
2316             MLocalLoopFlg=-1
2317             Break
2318             '
2319         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2320             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2321             fnKEY_WAIT = MRobotInit2%
2322             MLocalLoopFlg=-1
2323             Break
2324             '
2325         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2326             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2327             fnKEY_WAIT = MRobotInit3%
2328             MLocalLoopFlg=-1
2329             Break
2330             '
2331         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2332             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2333             fnKEY_WAIT = MRobotInit4%
2334             MLocalLoopFlg=-1
2335             Break
2336             '
2337         Else
2338         EndIf
2339     WEnd
2340     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2341     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2342 FEnd
2343 '
2344 '■ fnAUTO_CTL
2345 ''' <summary>
2346 ''' AUTOモードOFF、PLCからの開始待ち
2347 ''' </summary>
2348 ''' <remarks>
2349 ''' Date   : 2021/07/07 : M.Hayakawa
2350 ''' </remarks>
2351 Function M% fnAUTO_CTL
2352     fnAUTO_CTL = 0
2353     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2354     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2355     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2356     '
2357     If M_Svo=0 Then             'サーボON確認
2358         Servo On
2359     EndIf
2360     Wait M_Svo=1
2361 FEnd
2362 '
2363 '■ fnWindScreenOpen
2364 ''' <summary>
2365 ''' ウィンド画面の表示、非表示設定
2366 ''' </summary>
2367 '''<param name="%"></param>
2368 '''<param name="%"></param>
2369 '''<param name="%"></param>
2370 '''<param name="%"></param>
2371 ''' <remarks>
2372 ''' コメントD1001, D1002, D1003の設定
2373 ''' MWindReSet = 0     画面非表示
2374 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2375 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2376 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2377 ''' Date   : 2021/07/07 : M.Hayakawa
2378 ''' </remarks>
2379 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2380     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2381         M_Out16(12480) = MCommentD1001            'D1001 コメント
2382     EndIf
2383     '
2384     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2385         M_Out16(12496) = MCommentD1002            'D1002 コメント
2386     EndIf
2387     '
2388     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2389        M_Out16(12512) = MCommentD1003            'D1003 コメント
2390     EndIf
2391     '
2392     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2393     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2394     Dly 0.5
2395     M_Out(12363) = 0                         'ウィンド画面設定
2396 FEnd
2397 '
2398 '■FnCtlValue2
2399 ''' <summary>
2400 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2401 ''' </summary>
2402 ''' <param name="MCtlNo%"></param>
2403 ''' <remarks>
2404 ''' Date : 2022/04/28 渡辺
2405 ''' </remarks>
2406 '''
2407 '''  1：投入数       ＋１
2408 '''  2：組立ＯＫ数   ＋１
2409 '''  3：組立ＮＧ数   ＋１ (未使用)
2410 '''  4：吸着エラー数 ＋１
2411 ''' 99：読書開始信号 OFF
2412 '''
2413 Function M% FnCtlValue2(ByVal MCtlNo%)
2414     FnCtlValue2 = 1
2415     Select MCtlNo%
2416         Case 1        '投入数＋１
2417             M_Out(12569) = 0             '書込み開始信号OFF
2418             M_Out(12568) = 1             '読込み開始信号ON
2419             MInputQty = M_In16(11600)    '投入数受信
2420             MInputQty = MInputQty + 1    '投入数＋１
2421             M_Out16(12592) = MInputQty   '投入数送信
2422             M_Out(12569) = 1             '書込み開始信号ON
2423             Break
2424             '
2425         Case 2        '組立ＯＫ数＋１
2426             M_Out(12569) = 0             '書込み開始信号OFF
2427             M_Out(12568) = 1             '読込み開始信号ON
2428             MAssyOkQty = M_In16(11616)   '組立OK数受信
2429             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2430             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2431             M_Out(12569) = 1             '書込み開始信号ON
2432             Break
2433             '
2434         Case 4        '吸着エラー数＋１
2435             M_Out(12569) = 0                       '書込み開始信号OFF
2436             M_Out(12568) = 1                       '読込み開始信号ON
2437             MSuctionErrQty = M_In16(11648)         '吸着エラー数受信
2438             MSuctionErrQty = MSuctionErrQty + 1    '吸着エラー数＋１
2439             M_Out16(12640) = MSuctionErrQty        '吸着エラー数送信
2440             M_Out(12569) = 1                       '書込み開始信号ON
2441             Break
2442             '
2443         Case 99        '読書開始信号OFF
2444             M_Out(12568) = 0        '読込み開始信号OFF
2445             M_Out(12569) = 0        '書込み開始信号OFF
2446             Break
2447             '
2448     End Select
2449     Exit Function
2450 FEnd
2451 'Insightによる画像処理検査実行（並列処理なし）
2452 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2453 '-------------------------------------------------------------------------------
2454 'Insightによる画像処理検査実行（並列処理なし）
2455 '   引数
2456 '       PInspPos()      ：検査位置
2457 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2458 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2459 '       MInspCnt%       ：検査位置数
2460 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2461 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2462 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2463 '   戻り値：整数
2464 '       0=異常終了、1=正常終了
2465 '
2466 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2467 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2468 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2469 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2470 '   20200410    :   検査グループ設定Retry追加
2471 '-------------------------------------------------------------------------------
2472     '----- 初期設定 -----
2473     Cnt 0                                                           '移動効率化解除(初期値=0)
2474     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2475 '    Cnt 1,0.1,0.1
2476     '変数宣言・初期化
2477     Def Inte MNum                                                   '検査番号(検査順1～)
2478     MNum% = 1                                                       '検査番号初期値設定
2479     Def Inte MEndFlg                                                '検査終了フラグ
2480     MEndFlg% = 0
2481     '
2482     '検査G番号設定要求・検査実行要求off
2483     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2484     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2485     'エラー番号クリア
2486     MInspErrNum = 0                                                 '検査実行エラー番号
2487     M_Out16(MOUT_InspErrNum) = MInspErrNum
2488     MInspNGStepNum = 0                                              '検査実行NGStep番号
2489     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2490     '
2491     'Insight Ready check?
2492     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2493         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2494         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2495         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2496         ISInspectionSingle = 0                                      '異常終了戻り値設定
2497         Exit Function
2498     EndIf
2499     '
2500     '検査位置数確認
2501     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2502         MInspErrNum = 21                                            '検査データなし 21　引数<1
2503         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2504         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2505         ISInspectionSingle = 0                                      '異常終了戻り値設定
2506         Exit Function
2507     EndIf
2508     '
2509     '
2510     '
2511     '----- メイン処理 -----
2512     '設定された検査位置数分の検査実行
2513     While( MEndFlg% = 0 )
2514         '----- 検査グループ番号設定Retry追加 20200410
2515         MSetGrNumRetryExitFlg = 0
2516         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2517         While( MSetGrNumRetryExitFlg = 0 )
2518         '----- 検査グループ番号設定Retry追加ここまで 20200410
2519             '
2520             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2521             '
2522             '----- 検査グループ番号設定 -----
2523             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2524             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2525             '
2526             '検査位置へ移動・移動完了待ち
2527             Mvs PInspPos( MNum% )                                       '移動
2528             Dly 0.05                                                    '移動完了後Delay
2529             '
2530             '検査グループ番号設定終了確認
2531             M_Timer(1) = 0
2532             MExitFlg = 0
2533             While( MExitFlg = 0 )
2534                 '検査G設定正常終了?
2535                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2536                     MExitFlg = 1
2537                 '
2538                 '検査G設定異常終了?
2539                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2540                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2541                     If MInspErrNum = 0 Then                             '1回目のエラー?
2542                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
2543                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2544                     EndIf
2545                     MExitFlg = 1
2546                 '
2547                 'timeoutチェック
2548                 ElseIf 1000 < M_Timer(1) Then
2549                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
2550                     If MInspErrNum = 0 Then                             '1回目のエラー?
2551                         MInspErrNum = 12                                'timeout エラー番号=12
2552                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
2553                     EndIf
2554                     MExitFlg = 1
2555                 EndIf
2556             WEnd
2557             '
2558             '検査G番号設定要求off
2559             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
2560             '
2561             '----- 検査グループ設定Retry追加 20200410
2562             'NGなければ抜ける
2563             If MCurrentStepErr = 0 Then
2564                 MSetGrNumRetryExitFlg = 1
2565             Else
2566                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2567                 If MSetGrNumRetryCnt = 0 Then
2568                     MSetGrNumRetryExitFlg = 1
2569                 Else
2570                     'Retryへ　その前にDelay
2571                     Dly 0.5
2572                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2573                 EndIf
2574             EndIf
2575             '----- 検査グループ設定Retry追加ここまで 20200410
2576             '
2577         WEnd
2578         '
2579         '
2580         '
2581         '----- 検査実行 -----
2582         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
2583             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
2584                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
2585                 MInspRetryExitFlg = 0
2586                 MRetryCnt = 2                                        'Retry回数設定
2587                 While( MInspRetryExitFlg = 0 )
2588                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
2589                     '
2590                     '検査完了確認
2591                     MRetryCnt = MRetryCnt - 1
2592                     M_Timer(1) = 0
2593                     MExitFlg = 0
2594                     While( MExitFlg = 0 )
2595                     '検査完了待ち
2596                         '検査OK終了?
2597                         If M_In( MIN_IS_InspOK% ) = 1  Then
2598                             MJudgeOKFlg = 1                         '検査OKフラグON
2599                             MExitFlg = 1
2600                         '
2601                         '検査NG終了?
2602                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2603                             If MInspErrNum = 0 Then                 '1回目のエラー?
2604                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2605                                     MInspErrNum = 32                    '検査NG エラー番号=32
2606                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2607                                 EndIf
2608                             EndIf
2609                             MExitFlg = 1
2610                         '
2611                         '検査異常終了(IS timeout)?
2612                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2613                             If MInspErrNum = 0 Then                 '1回目のエラー?
2614                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2615                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
2616                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2617                                 EndIf
2618                             EndIf
2619                             MExitFlg = 1
2620                         '
2621                         'timeoutチェック
2622                         ElseIf 3000 < M_Timer(1) Then
2623                             If MInspErrNum = 0 Then                 '1回目のエラー?
2624                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
2625                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
2626                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
2627                                 EndIf
2628                             EndIf
2629                             MExitFlg = 1
2630                         EndIf
2631                     WEnd
2632                     '
2633                     '検査開始要求off
2634                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
2635                     '
2636                     'OKなら抜ける
2637                     If MJudgeOKFlg = 1 Then
2638                         MInspRetryExitFlg = 1
2639                     Else
2640                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
2641                         If MRetryCnt = 0 Then
2642                             MInspRetryExitFlg = 1
2643                         Else
2644                             'Retryへ　その前にDelay
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
2655         MNum% = MNum% + 1                                           '検査Step+1
2656         '検査終了確認　検査終了フラグセット
2657         If (MInspCnt% < MNum% ) Then
2658             MEndFlg% = 1                                            '検査終了フラグセット
2659         EndIf
2660         'NG発生時続行時処理
2661         If MInspErrNum <> 0 Then                                    'NGあり?
2662             If MNgContinue% <> 1 Then                               'NG続行?
2663                 MEndFlg% = 1                                        '検査終了フラグセット
2664             EndIf
2665         EndIf
2666     WEnd
2667     '
2668     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2669     If 0 < MZAxis% Then
2670         PCurrentPos = P_Curr                                        '現在位置取得
2671         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
2672         Mvs PCurrentPos                                             '現在位置上空へ移動
2673     EndIf
2674     Fine 0 , P
2675     '
2676     '戻り値設定
2677     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
2678         ISInspectionSingle = 1                                      '正常終了戻り値設定
2679     Else
2680         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2681         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2682         ISInspectionSingle = 0                                      '異常終了戻り値設定
2683     EndIf
2684     '
2685 FEnd
2686 '
2687 ' ■ISInspection
2688 ''' <summary>
2689 ''' Insightによる画像処理検査実行
2690 ''' </summary>
2691 '''<param name="PInspPos()">検査位置</param>
2692 '''<param name="MInspGrNum%()">検査位置での検査グループ番号（=0：画像検査未実施）</param>
2693 '''             PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2694 '''<param name="MInspCnt%">検査位置数</param>
2695 '''<param name="MZAxis%">終了時のZ軸退避座標（-1:無効）</param>
2696 '''             終了時にZ軸をMZAxisで設定された位置まで上昇させる
2697 '''<param name="MNgContinue%">=1で検査エラー・NG発生時に全Stepの検査を行う</param>
2698 '''<returns>    整数 0=異常終了、1=正常終了</returns>
2699 '''         MInspErrNum     ：異常終了時にエラー番号が設定される
2700 '''         MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される"
2701 ''' <remarks>
2702 ''' Date   : 2021/07/07 : M.Hayakawa
2703 ''' </remarks>
2704 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2705 '    '画像使用確認 0<- 画像確認無しの場合
2706 '    If M_In(11369) = 0 Then            'toRBT_使用確認
2707 '        ISInspection = 1                                        '正常終了戻り値設定
2708 '    EndIf
2709 ''
2710 '    Cnt 0                                                       '移動効率化解除(初期値=0)
2711 '    Fine 0.05,P                                                 '位置決め完了条件設置　0.05mm
2712 '    MNum% = 1                                                   '検査番号初期値設定
2713 '    Def Inte MEndFlg                                            '検査終了フラグ
2714 '    MEndFlg% = 0
2715 '    '
2716 '    'エラー番号クリア
2717 '    MInspErrNumSub = 0                                          '検査実行エラー番号sub
2718 '    MInspErrNum = 0                                             '検査実行エラー番号
2719 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2720 '    MInspNGStepNum = 0                                          '検査実行NGStep番号
2721 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2722 '    '
2723 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready offなら終了
2724 '        MInspErrNum = 20                                        '検査実行エラー番号 20 Insight offline
2725 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2726 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2727 '        ISInspection = 0                                        '異常終了戻り値設定
2728 ''
2729 '    EndIf
2730 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2731 '    '
2732 '    '検査位置数確認
2733 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2734 '        MInspErrNum = 21                                        '検査データなし 21　引数<1
2735 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2736 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2737 '        ISInspection = 0                                        '異常終了戻り値設定
2738 ''
2739 '    EndIf
2740 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2741 '    '
2742 '    '設定された検査位置数分の検査実行
2743 '    While( MEndFlg% = 0 )
2744 '        '検査終了確認　検査終了フラグセット
2745 '        If (MInspCnt% < MNum% ) Then
2746 '            MEndFlg% = 1                                        '検査終了フラグセット
2747 '        EndIf
2748 '        '
2749 '        'タスク　検査G番号設定・検査完了確認処理開始　INSPTAST1
2750 '        If MEndFlg% = 0 Then
2751 '            M_01# = MInspGrNum%(MNum%)                          '検査G番号引渡し
2752 '        EndIf
2753 '        M_02# = MEndFlg%                                        '検査終了フラグ引渡し
2754 '        M_05# = MNum%                                           '検査番号(検査順1～)
2755 '        'タスク　検査G設定フラグ引渡し
2756 '        If MEndFlg% = 0 Then
2757 '            If 0 < MInspGrNum%(MNum%) Then
2758 '                M_03# = 1
2759 '            Else
2760 '                M_03# = 0
2761 '            EndIf
2762 '        Else
2763 '            M_03# = 0
2764 '        EndIf
2765 '        'タスク　検査結果確認フラグ引渡し
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
2776 '        'タスク処理開始
2777 '        M_00# = 1                                               'TASK処理開始
2778 '        'タスク処理開始確認
2779 '        M_Timer(1) = 0
2780 '        MExitFlg = 0
2781 '        While( MExitFlg = 0 )
2782 '            '処理開始完了確認
2783 '            If M_00# = 0 And M_10# = 8 Then
2784 '                MExitFlg = 1
2785 '            EndIf
2786 '            'timeoutチェック
2787 '            If 2000 < M_Timer(1) Then
2788 '                If MNgContinue% = 1 Then                        'NG続行?
2789 '                    MInspErrNumSub = 36                         'エラー番号設定36
2790 '                Else
2791 '                    MInspErrNum = 36                            'エラー番号設定36
2792 '                EndIf
2793 '                MExitFlg = 1
2794 '            EndIf
2795 '        WEnd
2796 '        '
2797 '        '検査位置へ移動・移動完了待ち
2798 '        If 0 = MInspErrNum Then
2799 '            If MEndFlg% = 0 Then
2800 '                Mvs PInspPos( MNum% )                           '移動
2801 '            EndIf
2802 '        EndIf
2803 '        '
2804 '        'タスク　検査G番号設定・検査完了確認処理終了待ち　INSPTAST1
2805 '        If 0 = MInspErrNum Then
2806 '            M_Timer(1) = 0
2807 '            MExitFlg = 0
2808 '            While( MExitFlg = 0 )
2809 '                '処理完了待ち（正常終了）
2810 '                If M_10# = 1 Then
2811 '                    MExitFlg = 1
2812 '                EndIf
2813 '                '処理完了待ち（異常終了）
2814 '                If M_10# = 0 Then
2815 '                    If MNgContinue% = 1 Then                    'NG続行?
2816 '                        MInspErrNumSub = M_12#                  'エラー番号設定　M12
2817 '                    Else
2818 '                        MInspErrNum = M_12#                     'エラー番号設定　M12
2819 '                    EndIf
2820 '                    MExitFlg = 1
2821 '                EndIf
2822 '                'timeoutチェック
2823 '                If 5000 < M_Timer(1) Then
2824 '                    If MNgContinue% = 1 Then                    'NG続行?
2825 '                        MInspErrNumSub = 31                     'エラー番号設定31
2826 '                    Else
2827 '                        MInspErrNum = 31                        'エラー番号設定31
2828 '                    EndIf
2829 '                    MExitFlg = 1
2830 '                EndIf
2831 '            WEnd
2832 '        EndIf
2833 '        '
2834 '        '検査結果確認
2835 '        If 0 = MInspErrNum Then
2836 '            If 1 < MNum% Then
2837 '                If 0 < MInspGrNum%(MNum%-1) Then                '検査あり?
2838 '                    If M_11# = 2 Then                           '検査NG?
2839 '                        If MNgContinue% = 1 Then                'NG続行?
2840 '                            If MInspNGStepNum = 0 Then          'NG未発生?
2841 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2842 '                            EndIf
2843 '                            MInspErrNumSub = 32                 'エラー番号設定 32:検査NG
2844 '                        Else
2845 ''                            MInspNGStepNum = MNum% - 1          '検査実行NGStep番号設定
2846 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '検査実行NG　検査G番号設定
2847 '                            MInspErrNum = 32                    'エラー番号設定 32:検査NG
2848 '                        EndIf
2849 '                   EndIf
2850 '                EndIf
2851 '            EndIf
2852 '        EndIf
2853 '        '
2854 '        'エラーなら検査中断終了するのでLoopから抜けるため終了フラグセット
2855 '        If 0 <> MInspErrNum Then
2856 '            MEndFlg% = 1
2857 '        EndIf
2858 '        '
2859 '        '検査実行、取込完了待ち
2860 '        If 0 = MInspErrNum Then
2861 '            If MEndFlg% = 0 Then
2862 '                If 0 < MInspGrNum%(MNum%) Then                  '検査あり?
2863 '                    M_Out(MOUT_IS_Insp%) = 1                    '検査実行要求on
2864 '                    '取込完了確認
2865 '                    M_Timer(1) = 0
2866 '                    MExitFlg = 0
2867 '                    While( MExitFlg = 0 )
2868 '                        '処理完了待ち
2869 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2870 '                            MExitFlg = 1
2871 '                        EndIf
2872 '                        'timeoutチェック
2873 '                        If 2000 < M_Timer(1) Then
2874 '                            If MNgContinue% = 1 Then            'NG続行?
2875 '                                MInspErrNumSub = 33             'エラー番号設定33
2876 '                            Else
2877 '                                MInspErrNum = 33                'エラー番号設定33
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
2889 '    '終了時にZ軸をMZAxisで設定された位置まで上昇させる
2890 '    If 0 < MZAxis% Then
2891 '        PCurrentPos = P_Curr                                    '現在位置取得
2892 '        PCurrentPos.Z = MZAxis%                                 'Z軸を設定
2893 '        Mvs PCurrentPos                                         '現在位置上空へ移動
2894 '    EndIf
2895 '    '
2896 '    'NG続行時処理
2897 '    If MNgContinue% = 1 Then                                    'NG続行?
2898 '        MInspErrNum = MInspErrNumSub                            'エラー番号設定
2899 '    EndIf
2900 '    '
2901 '    '戻り値設定
2902 '    If MInspErrNum = 0 Then
2903 '        ISInspection = 1                                        '正常終了戻り値設定
2904 '    Else
2905 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '検査実行エラー番号出力
2906 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '検査実行NGStep番号出力
2907 '        ISInspection = 0                                        '異常終了戻り値設定
2908 '    EndIf
2909 '    '
2910 '*ISInspection_End
2911 'FEnd
2912 '
2913 '■InitialZoneB
2914 ''' <summary>
2915 ''' 非常停止後の復帰動作
2916 ''' 1)上空退避　Z方向上に移動
2917 ''' 2)J1軸以外を退避ポジションへ移動
2918 ''' 3)J1軸のみを退避ポジションへ移動
2919 ''' 4)イニシャルポジションへ移動
2920 ''' </summary>
2921 ''' <remarks>
2922 ''' Date : 2022/03/23 : N.Watanabe
2923 ''' </remarks>
2924 Function V fnInitialZoneB()
2925     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/04/26 渡辺
2926 '
2927 'パラメータ
2928     Ovrd 5
2929 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2930 '    Cmp Pos, &B100011
2931 '
2932 '復帰動作開始
2933 '
2934 '置き台と両掴みの場所は、チャックを解放する
2935 *RecoveryChuckOpen
2936     PActive = P_Curr          '現在位置を取得
2937     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
2938 'PProductOnRoboSet(ねじロボ製品置き位置)は、チャック解放
2939     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2940         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2941             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2942                 MRecoveryChuckOpen = 1
2943             EndIf
2944         EndIf
2945     EndIf
2946 'PProductOnRoboGet(ねじロボ製品取り位置)は、チャック解放
2947     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2948         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2949             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2950                 MRecoveryChuckOpen = 1
2951             EndIf
2952         EndIf
2953     EndIf
2954 '
2955     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2956     M_Out(12256) = 0                           '本体チャック閉OFF
2957     M_Out(12257) = 1                           '本体チャック開ON
2958 '
2959     M_20# = 0                                  'KEY入力初期化
2960     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '本体チャック開検出
2961     If MRtn = 1 Then M_Out(12257) = 0          '本体チャック開OFF
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
2972 '背面板回避
2973 'PPlateBackSet～PPlateBackSet_6のエリアにいるときは、本体チャック開く
2974 '・PPlateBackSet_6         '経路6
2975 '・PPlateBackSet_5         '経路7
2976 '・PPlateBackSet_4         '経路8
2977 '・PPlateBackSet_3         '経路9
2978 '・PPlateBackSet_2         '経路10
2979 '・PPlateBackSet_1         '経路11
2980 '・PPlateBackSet           '背面板置き位置
2981 '上記７点のＸ座標・Ｙ座標・Ｚ座標とJ6軸が下記If文の範囲に入っている事を確認する事
2982     PActive = P_Curr                    '現在位置を取得
2983     JActive = J_Curr                    '現在位置を取得
2984     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
2985     If (PActive.X >= -35) And (PActive.X <= -5) Then
2986         If (PActive.Y >= 350) And (PActive.Y <= 515) Then
2987             If (PActive.Z >= 480) And (PActive.Z <= 560) Then
2988                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2989                     M_Out(12256) = 0            '本体チャック閉OFF
2990                     M_Out(12257) = 1            '本体チャック開ON
2991                 Dly 1.0
2992                 EndIf
2993             EndIf
2994         EndIf
2995     EndIf
2996 '
2997 '
2998 '特殊回避　直接、上空退避が出来ない所の対処
2999 '
3000     Ovrd 1
3001 'PProductOnRoboSet(Get)～PProductOnRoboSet(Get)_2のエリアにいるときは、PProductOnRoboSet_2へ
3002 '・PProductOnRoboSet
3003 '・PProductOnRoboSet_1
3004 '・PProductOnRoboSet_2
3005 '・PProductOnRoboGet
3006 '・PProductOnRoboGet_1
3007 '・PProductOnRoboGet_2
3008 '上記６点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
3009     PActive = P_Curr                    '現在位置を取得
3010     JActive = J_Curr                    '現在位置を取得
3011     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
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
3027 'PProductOnRoboSet(Get)_2～PProductOnRoboSet(Get)_3のエリアにいるときは、PProductOnRoboSet_3へ
3028 '・PProductOnRoboSet_2
3029 '・PProductOnRoboSet_3
3030 '・PProductOnRoboGet_2
3031 '・PProductOnRoboGet_3
3032 '上記４点のＸ座標・Ｙ座標・Ｚ座標が下記If文の範囲に入っている事を確認する事
3033     PActive = P_Curr                    '現在位置を取得
3034     JActive = J_Curr                    '現在位置を取得
3035     MJ6 = Deg(JActive.J6)               'J6軸の値を比較する為に代入
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
3049 '上空退避
3050     PActive = P_Curr
3051     Pmove = PActive
3052     Pmove.Z = 640           '上空退避する一律の高さ
3053     If PActive.X > 550 Then
3054         Pmove.Z =550        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
3055     EndIf
3056     If PActive.Z < Pmove.Z Then
3057         Mvs Pmove
3058     EndIf
3059     Dly 1.0
3060 'J1軸以外を退避ポジションへ移動
3061     JActive = J_Curr
3062     Jmove = JTaihi
3063     Jmove.J1 = JActive.J1        'J1軸は現在値を使用し、JTaihiのポーズを取る
3064     Jmove.J6 = JActive.J6        'J6軸は現在値を使用し、JTaihiのポーズを取る
3065     Mov Jmove
3066     Dly 1.0
3067 'J1軸のみを退避ポジションへ移動
3068     Mov JTaihi
3069     Dly 1.0
3070 'イニシャルポジションへ移動
3071     Mov PInitialPosition
3072     Cmp Off
3073     Ovrd 100
3074 ' ねじロボを初期位置に戻すために強制的に自動運転開始
3075     If M_In(11856) = 0 Then                 ' 停止中のみ
3076         fnAutoScreenComment(501)            ' 状態表示[ネジ締め機自動運転開始中] 2022/04/25 渡辺
3077         M_Out(12834) = 1                    ' 自動運転開始ON 12834   M6834
3078         MRet = frInCheck(11842, 1, 30000&)  ' 自動運転開始受信待ち    11842   M5842
3079         If MRet = 0 Then
3080         Else
3081             M_Out(12834) = 0    ' 自動運転開始OFF 12834   M6834
3082         EndIf
3083     EndIf
3084     M_Out(12262) = 0            '位置決め出OFF
3085     M_Out(12263) = 1            '位置決め戻ON
3086     fErrorProcess(11,253,281,0)
3087 *RecoveryEnd
3088     Exit Function
3089 FEnd
3090 '
3091 '
3092 '■fnAutoScreenComment
3093 ''' <summary>
3094 ''' メイン画面の動作状況表示
3095 ''' コメントD1005の設定
3096 ''' </summary>
3097 '''<param name="McommentD1005%">コメントID</param>
3098 ''' <remarks>
3099 ''' Date   : 2021/07/07 : M.Hayakawa
3100 ''' </remarks>
3101 Function fnAutoScreenComment(ByVal McommentD1005%)
3102     M_Out16(12576) = McommentD1005%
3103 FEnd
3104 '
3105 '■fnRoboPosChk
3106 ''' <summary>
3107 ''' 最後に終了したロボットポジションの確認
3108 ''' </summary>
3109 '''<param name="MINNumber%">入力番号</param>
3110 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3111 '''<param name="MTimeCnt&">タイムアウト時間</param>
3112 ''' PLCに保続した番号を読込み、確認
3113 ''' MRBTOpeGroupNo = 5 が初期位置に設定
3114 '''<returns>整数 0:タイムアウト 1:OK</returns>
3115 ''' <remarks>
3116 ''' Date   : 2021/07/07 : M.Hayakawa
3117 ''' </remarks>
3118 Function M% fnRoboPosChk
3119     fnRoboPosChk = 0
3120     MRet = fnStepRead()
3121     '初期位置でないと判断した場合
3122     'ウィンド画面切換え
3123     If MRBTOpeGroupNo > 5 Then
3124         '下記キー待ちの継続に反応させないため
3125         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3126         Dly 0.2
3127         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3128         Dly 1.5
3129         '
3130         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
3131         '
3132         MLoopFlg% = 1
3133         While MLoopFlg% = 1
3134             '
3135             '
3136             MKeyNumber% = fnKEY_WAIT()
3137             Select MKeyNumber%
3138                 Case Is = MAbout%       '停止
3139                     M_20# = MAbout%
3140                     MLoopFlg% = -1
3141                     Break
3142                 Case Is = MNext%        '次へ
3143                     'MLoopFlg% = -1
3144                     Break
3145                 Case Is = MContinue%    '継続
3146                     M_20# = MContinue%
3147                     MLoopFlg% = -1
3148                     Break
3149                 Default
3150                     Break
3151             End Select
3152         WEnd
3153     EndIf
3154     '
3155     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3156         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3157         Ovrd 5                                   '低速オーバーライド値設定
3158         Select MRBTOpeGroupNo
3159             Case Is = 5                          '何もしない
3160                 Break
3161             Case Is = 10                         '初期位置へ戻す
3162                 'Mov PTEST001
3163                 Break
3164             Case Is = 15                         '初期位置へ戻す
3165                 'Mov PTEST002
3166                 Dly 0.5
3167                 'Mov PTEST001
3168                 Dly 0.5
3169                 Break
3170             Default
3171                 Break
3172         End Select
3173         '
3174         Ovrd M_NOvrd                            'システムの初期値を設定
3175         M_Out(12364) = 1                        'toPLC_データ保存ON
3176         MRBTOpeGroupNo = 5
3177         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3178         Dly 1.0
3179         M_Out(12364) = 0                        'toPLC_データ保存OFF
3180         fnRoboPosChk = 1                        '初期位置動作実行
3181         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3182     EndIf
3183     Exit Function
3184 FEnd
3185 '
3186 '■frInCheck
3187 ''' <summary>
3188 ''' センサーINチェック
3189 ''' </summary>
3190 '''<param name="MINNumber%">入力番号</param>
3191 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3192 '''<param name="MTimeCnt&">タイムアウト時間</param>
3193 '''<returns>整数 0:タイムアウト 1:OK</returns>
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
3213 'ねじ締め機通信確認
3214 '
3215 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3216 'fScrewTcomChk = 0　：正常終了
3217 '          　 　 -1 ：異常終了
3218 '-----------------------------------------------
3219 Function M% fScrewTcomChk
3220 *ReCheckScewTcomChk
3221     fScrewTcomChk = 0
3222     '通信確認送信
3223     M_Out(MOUT_ScwT_ComChk%) = MOn%
3224     '通信確認受信待機
3225 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3226     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3227     '通信確認送信終了
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
3239 'ねじ締め開始送信
3240 '
3241 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3242 'fScrewTStart = 0　：正常終了
3243 '           　　-1 ：異常終了
3244 '-----------------------------------------------
3245 Function M% fScrewTStart
3246     fScrewTStart = 0
3247     nRet% = 0
3248     'ねじ締め開始待機を受信
3249 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3250     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3251     If MRtn = 0 Then nRet% = -1
3252     If MRtn = 0 Then GoTo *ScrewStartERROR      '開始できなかった場合ジャンプ
3253     Dly 0.1
3254     'ねじ締め開始受信を送信
3255     M_Out(MOUT_ScwT_ST%) = MOn%
3256     Dly 0.5
3257     'Wait M_In(MTEST_KEY%) = MOn%
3258     'ねじ締め開始送信終了
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
3269 'ねじ締め完了受信
3270 '
3271 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3272 'fScewTFinish = 0　：正常終了
3273 '          　 　-1 ：異常終了
3274 '-----------------------------------------------
3275 Function M% fScewTFinish
3276 *ReCheckScewTFinish
3277     fScewTFinish = 0
3278     'ねじ締め完了待機を受信
3279 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3280     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3281     If MRtn = 0 Then
3282         fScewTFinish = -1
3283     EndIf
3284     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3285     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3286     Dly 0.1
3287     'ねじ締め完了受信を送信
3288     M_Out(MOUT_ScwT_FinOK%) = MOn%
3289     Dly 0.5                          'とりあえず保持時間0.5msec
3290     'ねじ締め開始送信終了
3291     M_Out(MOUT_ScwT_FinOK%) = MOff%
3292     'Wait M_In(MTEST_KEY%) = MOn%
3293     '
3294 *ScewTFinish_ErrEnd
3295 FEnd
3296 '
3297 '
3298 '-----------------------------------------------
3299 '
3300 '条件xx停止受信
3301 '
3302 '22/09/29 Waitがタイムアウトできるよう修正(中村)
3303 'fScewTCaseStop = 0　：正常終了
3304 '          　   　-1 ：異常終了
3305 '-----------------------------------------------
3306 Function M% fScewTCaseStop(ByVal MCase%())
3307 *ReCheckScewTCaseStop
3308     fScewTCaseStop = 0
3309     '条件xx停止を受信
3310     Wait M_In(MCase%(1)) = MOn%
3311     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3312     If MRtn = 0 Then
3313         fScewTCaseStop = -1
3314     EndIf
3315     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3316     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3317     Dly 0.1
3318     '条件xx停止受信を送信
3319     M_Out(MCase%(2)) = MOn%
3320     Dly 0.5                          'とりあえず保持時間0.5msec
3321     'ねじ締め開始送信終了
3322     M_Out(MCase%(2)) = MOff%
3323 *ScewTCaseStop_ErrEnd
3324     '
3325 FEnd
3326 ''■fScrewTighenRoboCheck
3327 '<summary>
3328 'ねじロボ監視
3329 '</summary>
3330 '<param name = "MStopNum%"> 停止番号</param>
3331 '<returns>整数 0:ねじロボ異常終了 1:OK </returns>
3332 '<make>
3333 '2021/12/2 中村天哉
3334 '</make>
3335 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3336     fnAutoScreenComment(503)    '状態表示[ねじロボ動作終了待ち] 2022/04/26 渡辺
3337     fScrewTighenRoboCheck = 1
3338     MScrewTighenRoboFlg% = 1    'フラグの初期化
3339     MCheck% = 0
3340     While MScrewTighenRoboFlg% = 1
3341         MCheck% = M_In16(11904)
3342         If M_In(MStopNum%) = 1 Then '停止位置まで来たら
3343             MScrewTighenRoboFlg% = 0 '関数を抜ける
3344             fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/04/26 渡辺
3345         EndIf
3346         If MCheck% <> 0 Then
3347             fScrewTighenRoboError(MCheck%)
3348             Select M_20#
3349                 Case MAbout%            '停止が押された場合
3350                     M_Out(12869) = 1 Dly 1.0
3351                     MScrewTighenRoboFlg% = 0
3352                     fScrewTighenRoboCheck = 0   '異常終了
3353                     Break
3354                 Case MNgProcess%        'NGが押された場合
3355                     M_Out(12873) = 1 Dly 1.0
3356                     MScrewTighenRoboFlg% = 0
3357                     fScrewTighenRoboCheck = 0   '異常終了
3358                     Break
3359                 Case MContinue%             'リトライが押された場合
3360                     M_20# = MClear%         'M_20#初期化
3361                     M_Out(12871) = 1 Dly 1.0
3362                     Break
3363                 Case MNext%                 '次へが押された場合
3364                     M_20# = MClear%         'M_20#初期化
3365                     M_Out(12874) = 1 Dly 1.0
3366                     Break
3367             End Select
3368             Dly 0.5
3369         EndIf
3370     WEnd
3371 FEnd
3372 '
3373 '■fScrewTighenRoboError
3374 '<summary>
3375 'ねじロボエラー処理
3376 '</summary>
3377 '<param name = "ErrorCode%"> エラー番号</param>
3378 '<make>
3379 '2021/12/2 中村天哉
3380 '</make>
3381 Function fScrewTighenRoboError(ByVal MErrorCode%)
3382     MErrorScreenCode% = 0
3383     MErrorScreenCode% = MErrorCode% + 300
3384     fErrorProcess(11,MErrorScreenCode%,0,0)
3385 FEnd
3386 '
3387 '■fErrorProcess
3388 '<summary>
3389 'エラー処理
3390 '</summary>
3391 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3392 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3393 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3394 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3395 '<make>
3396 '2021/11/5 中村天哉
3397 '</make>
3398 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3399     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3400     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3401     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3402     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3403 *RETRY_ERR_PROCESS
3404      M_20# = MClear%     '初期化
3405 '        'エラー処理記述
3406         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3407 '        'GOT KEY入力待ち
3408         MKeyNumber = fnKEY_WAIT()
3409 '        '
3410         If MKeyNumber = MAbout% Then   '停止を選択した場合
3411             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3412             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3413             Break
3414          '
3415         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3416             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3417             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3418         '
3419         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3420             M_20# = MNext%            'M_20# プログラム間共通外部変数
3421             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3422          '
3423         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3424             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3425             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3426             Break
3427         '
3428         EndIf
3429         '
3430         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3431 FEnd
3432 '
3433 '■fnTorqueCheck
3434 ''' <summary>
3435 ''' トルクチェック動作用のメイン
3436 ''' </summary>
3437 ''' <remarks>
3438 ''' Date   : 2021/12/21 : H.AJI
3439 ''' </remarks>'
3440 Function M% fnTorqueCheck
3441     'トルクチェック中送信  搬送系停止
3442     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3443     '
3444     fnTorqueCheck = 0
3445     Ovrd 20
3446     Mov PInitialPosition              '初期位置移動
3447     Ovrd 100
3448     '下記キー待ちの継続に反応させないため
3449     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3450     Dly 0.2
3451     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3452     '
3453     'M6340  トルクチェック受信
3454     'Dly 5.0
3455     M_Out(12340) = 1          'トルクチェック受信 M6340
3456     Dly 1.0
3457     M_Out(12340) = 0
3458     '
3459     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3460     M_Out(12835) = 1                         'ねじロボトルクチェック画面切替
3461    Wait M_In(11843) = 1                         'ねじロボトルクチェック画面切替
3462     M_Out(12835) = 0                         'ねじロボトルクチェック画面切替完了
3463     '
3464     '
3465     MLoopFlg = 1
3466     While MLoopFlg = 1
3467         '
3468         Mov PInitialPosition              '初期位置移動
3469         '
3470         MKeyNumber = fnKEY_WAIT()
3471         Select MKeyNumber
3472             Case Is = 1           '停止
3473                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3474                 Dly 1.0
3475                 M_Out(12343) = 0
3476                 Ovrd 20
3477                 'Mov PTicketRead_1
3478                 M_Out(12840) = 1          'トルクチェック終了
3479                 Wait M_In(11859) = 1      'ねじロボからの終了
3480                 M_Out(12840) = 0          'トルクチェック終了
3481                 Ovrd 100
3482                 M_20# = 1
3483                 MLoopFlg = -1
3484                 Break
3485             Case Is = 2           '次へ
3486                 Break
3487             Case Is = 3           '継続
3488                 Break
3489             Case Is = 4           'トルクチェック開始
3490                 M_Out(12342) = 1          'トルクチェック開始要求受信 M6342
3491                 Dly 1.0
3492                 M_Out(12342) = 0
3493                 If M_In(11862) = 1 Then             'トルクチェッカー確認
3494                     fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3495                     MRet = fnScrewMTorque()           'ねじロボ用トルクチェック
3496                 EndIf
3497                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3498                 'MRet = fnMoveTorquePosi()
3499                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3500                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3501                 Break
3502             Default
3503                 Break
3504         End Select
3505     WEnd
3506     '
3507     'トルクチェック中停止送信
3508     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3509     '
3510     'ロボットの位置を元に戻す
3511     '
3512     '
3513  FEnd
3514  '
3515 '
3516 '
3517 '---------------------------
3518 '
3519 '    メイン画面の表示、非表示設定
3520 '         コメントD1001, D1002, D1003の設定
3521 '           MWindReSet = 0     画面非表示
3522 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3523 '           MWindErrScr = 10    エラー画面 D1001, D1002
3524 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3525 '
3526 '---------------------------
3527 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3528     fnMainScreenOpen = 0
3529     '
3530    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3531         M_Out16(12480) = MCommentD1001            'D1001 コメント
3532     EndIf
3533     '
3534     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3535         M_Out16(12496) = MCommentD1002            'D1002 コメント
3536     EndIf
3537     '
3538     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3539         M_Out16(12512) = MCommentD1003            'D1003 コメント
3540     EndIf
3541     '
3542     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3543     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3544     Dly 0.5
3545     M_Out(12362) = 0                         'ウィンド画面設定
3546 FEnd
3547 '
3548 '■Main
3549 ''' <summary>
3550 ''' トルクチェック実動作
3551 ''' </summary>
3552 ''' <remarks>
3553 ''' Date   : 2021/12/21 : H.AJI
3554 ''' </remarks>'
3555 Function M% fnScrewMTorque
3556     fnScrewMTorque = 0
3557     M_Out(12838) = 1                         'トルクチェック開始1
3558     Wait M_In(11857) = 1                     '受信完了
3559     M_Out(12838) = 0                         'トルクチェック開始1
3560     Dly 2.0
3561 FEnd
3562 '
3563 '
3564 '----------------------------------------------------------------
3565 'fTimeOutJudge
3566 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3567 '引数
3568 'Address% = 監視アドレス番号
3569 'JudgeFlg% = 対象アドレスの正常終了時の値
3570 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3571 '戻り値 = 0 エラー
3572 '         1 正常終了
3573 '         2 リトライ
3574 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3575 '作成日
3576 '2022/9/20 中村
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
3596 '■Main
3597 ''' <summary>
3598 ''' 組立動作用のメイン
3599 ''' </summary>
3600 ''' <remarks>
3601 ''' Date   : 2021/07/07 : M.Hayakawa
3602 ''' </remarks>'
3603 Function Main
3604     MopeNo = M_21#         '外部変数にて動作番号代入
3605     '
3606     If M_Svo=0 Then
3607         Servo On
3608     EndIf
3609     Wait M_Svo=1
3610 '組立スタート日付時刻要求パルスON
3611     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3612 'パトライト操作
3613     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3614     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3615     '
3616     M_20# = 0                                   'KEY入力初期化
3617     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3618     MRet% = 0
3619 '初期位置の確認と移動
3620 '
3621 '
3622 '復帰動作　実行・未実行判別      2022/03/22 渡辺 作成
3623     PActive = P_Curr                    '現在位置を取得
3624     MRecoveryPass% = 0
3625     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3626         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3627             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3628                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3629             EndIf
3630         EndIf
3631     EndIf
3632     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3633         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3634             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3635                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3636             EndIf
3637         EndIf
3638     EndIf
3639     If MRecoveryPass% = 0 Then
3640        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3641     EndIf
3642 '
3643 '
3644 '    MRet% = fnRoboPosChk()
3645 '    If MRet% = 1 Then                           '初期位置の動作を行った場合
3646 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3647 '        MKeyNumber% = fnKEY_WAIT()
3648 '        Select MKeyNumber%
3649 '            Case Is = MAbout%       '停止
3650 ''                M_20# = MAbout%
3651 '                MLoopFlg% = -1
3652 '                Break
3653 '            Case Is = MNext%        '次へ
3654 '                'MLoopFlg = -1
3655 '                Break
3656 '            Case Is = MContinue%    '継続
3657 '                M_20# = MContinue%
3658 '                MLoopFlg% = -1
3659 '                Break
3660 '            Default
3661 '                Break
3662 '        End Select
3663 '    EndIf
3664     '
3665     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3666         M_Out(12364) = 1            'toPLC_データ保存ON
3667 'トルクチェック
3668         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3669             MRet% = fnTorqueCheck()
3670             Break
3671         Else
3672 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3673 '                MRtn = InspInit()               '画像処理初期化処理
3674 '            EndIf
3675             '
3676            M_20# = MClear%                    '初期化
3677 '組立開始
3678             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3679                 fnAssyStart()
3680             Else
3681                 M_20# = MPass%
3682             EndIf
3683 '組立終了日付時刻
3684             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3685             Wait M_In(11572) = 1            '日付取得完了
3686             Dly 0.1
3687             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3688 'リフターユニットへのOUT
3689             '  KEY入力が何もない場合 OKと判断
3690             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3691             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3692 'OK/NGフラグ出力
3693             If M_20# <= 0 Then
3694                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3695             ElseIf M_20# = MPass% Then
3696                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3697             EndIf
3698 'PIASに組立完了書込み
3699             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3700                 If M_20# = MPass% Then
3701                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3702                 Else
3703                     'KEY入力がNGの場合
3704                     If M_20# = MNgProcess% Then
3705                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3706                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3707                         MRet% = fnPiasWrite(MNG%)
3708                        nAssyNgQty = nAssyNgQty + 1
3709                     EndIf
3710                     '
3711                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3712                     If M_20# = MAssyOK% Then
3713                             '-----------------------
3714                             'D732 -> D2600 コピー要求
3715                             M_Out(12566) = 1
3716 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3717                             M_Out(12566) = 0
3718                             '
3719                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3720                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3721                             '基板番号照合(PPは未使用）
3722 '                            MRet% = fnPCBNumberCheck()
3723                         Else
3724                             MRet% = 1
3725                         EndIf
3726                         '
3727                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3728                             If M_20# <> MAbout% Then
3729                                 '工程履歴OK書き込み
3730                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3731                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3732                                 MRet% = fnPiasWrite(MOK%)
3733                                 nAssyOkQty = 0
3734                                 nAssyOkQty = nAssyOkQty + 1
3735                             Else
3736                                 nAssyOkQty = nAssyOkQty + 1
3737                             EndIf
3738                         EndIf
3739                     EndIf
3740 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3741 '                    MRet% = fnPiasWrite(MOK%)
3742                 EndIf
3743             Else
3744                 nAssyOkQty = nAssyOkQty + 1
3745             EndIf
3746             '
3747             '組立終了日付時刻解除
3748             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3749             '投入数、組立OK数、組立NG数書込み
3750 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3751             '
3752 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3753 '                '画像処理終了処理
3754 '                MRtn = InspQuit()
3755 '            EndIf
3756         EndIf
3757         M_Out(12364) = 0                          'toPLC_データ保存OFF
3758     EndIf
3759 'パトライト操作
3760     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3761     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3762 'GOT表示
3763     fnAutoScreenComment(93)  'AUTO画面 工程完了
3764 FEnd
3765 End
3766 '
3767 'おまじないコメント
3768 '絶対削除するな
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
