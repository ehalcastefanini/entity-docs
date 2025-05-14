<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é criar uma interface de edição para gerenciar informações relacionadas a códigos de pagamento, incluindo descontos, datas de vencimento, e configurações adicionais como controle de checklist de pedidos. Ele fornece uma interface gráfica para entrada e validação de dados, além de integração com serviços externos para manipulação de dados.

* **Tecnologias Utilizadas:**
  - Delphi (VCL - Visual Component Library).
  - Componentes personalizados como `TsLabel`, `TsDBEdit`, `TsDBCheckBox`.
  - Integração com serviços SOAP via `SOAPHTTPClient`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `EDTpaymentCode`: Campo de texto vinculado ao banco de dados para o código de pagamento.
      - `EDTdiscount`: Campo de texto vinculado ao banco de dados para o desconto.
      - `EDTdueDate`: Campo de texto vinculado ao banco de dados para a data de vencimento.
      - `EDTdiscDate`: Campo de texto vinculado ao banco de dados para os dias de desconto.
      - `CHKdscfinOnDocs`: Caixa de seleção vinculada ao banco de dados para controle de desconto em documentos.
      - `CHKchkDispCred`: Caixa de seleção vinculada ao banco de dados para controle de crédito.
      - `CHKgiroPayment`: Caixa de seleção vinculada ao banco de dados para controle de pagamento via giro.
    - **Ações do Formulário e seus Efeitos:**
      - Validação de campos ao sair de um campo (`OnExit`).
      - Configuração de propriedades de busca e integração com serviços externos.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Entrada e validação de dados nos campos do formulário.
  - Configuração de buscas relacionadas a documentos e checklists.
  - Integração com serviços externos para manipulação de dados.

* **Componentes Principais:**
  - `EDTpaymentCode`: Campo para entrada do código de pagamento.
  - `FRAMEstatusInfo1`: Exibe informações de status relacionadas ao pagamento.
  - `FRAMEfindInvDisp` e `FRAMEFindordChklstdocs`: Componentes para busca de informações relacionadas a documentos e checklists.

* **Tradução para Pseudo-código:**
  - Evento `OnExit` do campo `EDTpaymentCode`: `se o campo for alterado, então validar o valor`.
  - Configuração de busca: `configurar fonte de dados e campos para busca`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do componente com a configuração de propriedades e integração com serviços.
  - Interação do usuário com os campos do formulário, acionando eventos como validação e busca.
  - Integração com serviços externos para manipulação de dados.

* **Dados Necessários:**
  - Código de pagamento.
  - Desconto.
  - Datas de vencimento e desconto.
  - Configurações adicionais como controle de checklist e pagamento via giro.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O campo `EDTpaymentCode` deve ser preenchido antes de salvar.
  - As caixas de seleção controlam configurações adicionais e devem ser configuradas conforme necessário.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validação de Campos:**
  - `EDTpaymentCode`: Deve ser validado ao sair do campo.
  - Outros campos não possuem validações explícitas definidas no código.

---

## 5. Funções Principais:

* **Funções e Lógica de Negócio:**
  - `m_SetFindInvDisp`: Configura a busca de informações relacionadas a documentos.
  - `m_SetFindOrdChklstdocs`: Configura a busca de informações relacionadas a checklists de pedidos.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `TPaymentServiceUtils`.
  - Propósito: Manipulação de dados relacionados a pagamentos.
  - Dados enviados e recebidos não estão explicitamente definidos no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `kneUtils`, `PaymentServiceUtils`, `InvoiceDispServiceUtils`, `DocCheckListServiceUtils`: Utilitários personalizados.

* **Componentes Personalizados:**
  - `TsLabel`, `TsDBEdit`, `TsDBCheckBox`: Componentes visuais personalizados.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `EDTpaymentCode` (tipo: string, obrigatório, não há validações explícitas definidas).
  - `EDTdiscount` (tipo: string, opcional, não há validações explícitas definidas).
  - `EDTdueDate` (tipo: string, opcional, não há validações explícitas definidas).
  - `EDTdiscDate` (tipo: string, opcional, não há validações explícitas definidas).
  - `CHKdscfinOnDocs` (tipo: boolean, opcional).
  - `CHKchkDispCred` (tipo: boolean, opcional).
  - `CHKgiroPayment` (tipo: boolean, opcional).

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```delphi
  procedure TFRAMEpaymentCode.EDTpaymentCodeExit(Sender: TObject);
  begin
    // Validação do código de pagamento
  end;
  ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades do componente na inicialização.
* Integração com serviços externos para manipulação de dados.

---

## 12. Conclusão:

O código fornece uma interface funcional para gerenciar informações de pagamento, com integração a serviços externos e validação básica de campos. No entanto, faltam mensagens de erro e validações mais robustas, o que pode limitar sua usabilidade em cenários mais complexos.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar códigos de pagamento, incluindo campos para desconto, datas e configurações adicionais. Ele integra serviços externos para manipulação de dados e validações básicas, mas carece de mensagens de erro e validações mais detalhadas.#### **FRpaymentCode.pas**

```
unit FRpaymentCode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, Mask, DBCtrls, sDBEdit, sLabel, kneFRStatusInfo,
  kneFRFindEditSOA, sCheckBox, sDBCheckBox;

type
  TFRAMEpaymentCode = class(TFRAMEBaseCtrlEditSOA)
    LBL1: TsLabel;
    EDTpaymentCode: TsDBEdit;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    sLabel2: TsLabel;
    EDTdiscount: TsDBEdit;
    sLabel3: TsLabel;
    EDTdueDate: TsDBEdit;
    sLabel4: TsLabel;
    sLabel5: TsLabel;
    EDTdiscDate: TsDBEdit;
    sLabel1: TsLabel;
    FRAMEfindInvDisp: TFRAMEFindEditSOA;
    CHKdscfinOnDocs: TsDBCheckBox;
    CHKchkDispCred: TsDBCheckBox;
    FRAMEFindordChklstdocs: TFRAMEFindEditSOA;
    sLabel6: TsLabel;
    CHKgiroPayment: TsDBCheckBox;
    procedure EDTpaymentCodeExit(Sender: TObject);
  private
    FOnBeforeApplyChanges: TNotifyEvent;
    { Private declarations }
    procedure m_SetFindInvDisp;
    procedure SetOnBeforeApplyChanges(const Value: TNotifyEvent);
    procedure m_SetFindOrdChklstdocs;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    property OnBeforeApplyChanges: TNotifyEvent read FOnBeforeApplyChanges write SetOnBeforeApplyChanges;

  end;

var
  FRAMEpaymentCode: TFRAMEpaymentCode;

implementation

uses
  kneUtils {#18555},
  PaymentServiceUtils, kneTypes, InvoiceDispServiceUtils, kneFREditSOA,
  DocCheckListServiceUtils {#18555};

{$R *.dfm}

{ TFRAMEpaymentCode }

constructor TFRAMEpaymentCode.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'Payment';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // Chamada dos Finds
  m_SetFindInvDisp;
  m_SetFindOrdChklstdocs; //2014/02/12, #18555

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TPaymentServiceUtils.Create(self);

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

end;

procedure TFRAMEpaymentCode.m_SetFindInvDisp;
begin
  with FRAMEfindInvDisp do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'FinvDisp';
    EditSettings.FieldNameForDesc := 'FInvDispDesc';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'invDispCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
```

#### **FRpaymentCode.dfm**

```
inherited FRAMEpaymentCode: TFRAMEpaymentCode
  Width = 663
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBL1: TsLabel [0]
    Left = 8
    Top = 13
    Width = 35
    Height = 13
    Caption = 'C&ode:'
    FocusControl = EDTpaymentCode
  end
  object sLabel2: TsLabel [1]
    Left = 8
    Top = 39
    Width = 54
    Height = 13
    Caption = 'D&iscount:'
    FocusControl = EDTdiscount
  end
  object sLabel3: TsLabel [2]
    Left = 173
    Top = 39
    Width = 61
    Height = 13
    Caption = 'Due Days:'
    FocusControl = EDTdueDate
  end
  object sLabel4: TsLabel [3]
    Left = 8
    Top = 143
    Width = 53
    Height = 13
    Caption = 'Inv Disp:'
  end
  object sLabel5: TsLabel [4]
    Left = 323
    Top = 39
    Width = 100
    Height = 13
    Caption = 'Da&ys of discount:'
    FocusControl = EDTdiscDate
  end
  object sLabel1: TsLabel [5]
    Left = 116
    Top = 39
    Width = 22
    Height = 13
    Caption = '(%)'
    FocusControl = EDTpaymentCode
  end
  object sLabel6: TsLabel [6]
    Left = 174
    Top = 13
    Width = 175
    Height = 13
    Caption = 'Controlled on Order Chec&klist:'
    FocusControl = FRAMEFindordChklstdocs.DBE
  end
  inherited PNLfooter: TsPanel
    Width = 663
    TabOrder = 10
    Visible = False
  end
  object EDTpaymentCode: TsDBEdit [8]
    Left = 64
    Top = 8
    Width = 73
    Height = 21
    AutoSize = False
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'paymentCode'
    DataSource = DStable
    TabOrder = 0
    OnExit = EDTpaymentCodeExit
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  inline FRAMEstatusInfo1: TFRAMEstatusInfo [9]
    Left = 8
    Top = 168
    Width = 466
    Height = 42
    AutoScroll = False
    ParentBackground = False
    TabOrder = 9
    inherited GRPstatus: TsGroupBox
      Width = 466
      inherited DBTXTlastUpd: TsDBText
        ParentFont = True
      end
```
<!-- tabs:end -->

