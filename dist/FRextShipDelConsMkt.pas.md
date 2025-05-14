<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um formulário para gerenciar informações relacionadas à entrega prévia de mercado (PreDelivery Information). Ele permite que o usuário configure alertas de tempo, dias e e-mails associados a essa funcionalidade. O objetivo é fornecer uma interface para configurar e validar essas informações de forma eficiente.

* **Tecnologias Utilizadas:**  
  - Delphi (VCL Framework).
  - Componentes visuais como `TsLabel`, `TsDBCheckBox`, `TsDBEdit`, `TcxDBTimeEdit`, e `TcxDBMaskEdit`.
  - Integração com banco de dados via `TDataSet` e `TClientDataSet`.

* **Tipo de Formulário:**  
  - **Formulário:**  
    - **Elementos do Formulário e Tipos:**  
      - `CHKcheckPreDeliv` (Checkbox): Permite ativar/desativar a funcionalidade de entrega prévia.  
      - `EDTcsaEmails` (Campo de texto): Campo para inserir e-mails associados.  
      - `EDTalertTime` (Campo de tempo): Campo para configurar o horário do alerta.  
      - `EDTalertDays` (Campo de máscara): Campo para configurar os dias do alerta.  
    - **Ações do Formulário e Efeitos:**  
      - Alterar o estado dos componentes com base na seleção do checkbox.  
      - Limpar os campos de alerta e e-mails quando a funcionalidade de entrega prévia é desativada.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**  
  - Ativar/desativar a funcionalidade de entrega prévia.  
  - Configurar alertas de tempo e dias.  
  - Inserir e-mails associados à funcionalidade.  

* **Componentes Principais:**  
  - `CHKcheckPreDeliv`: Controla a ativação da funcionalidade.  
  - `EDTcsaEmails`, `EDTalertTime`, `EDTalertDays`: Campos para entrada de dados relacionados aos alertas.  
  - `SetComponentesState`: Método que altera o estado dos componentes com base na ativação/desativação.  

* **Tradução para Pseudo-código:**  
  - Evento `OnClick` do checkbox:  
    ```pseudo
    se checkbox marcado então
        habilitar campos de alerta e e-mails
    senão
        desabilitar campos de alerta e e-mails
        limpar valores dos campos
    ```
  - Evento `AfterEdit` do dataset:  
    ```pseudo
    se dataset for editado então
        validar e salvar alterações
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**  
  1. Inicialização do formulário (`Create`):  
     - Configura propriedades como `MasterKeyFields`, `DataPacketName`, e `FrameType`.  
     - Define o estilo visual dos componentes.  
  2. Exibição de dados (`ShowData`):  
     - Atualiza o estado dos componentes com base no valor do checkbox.  
  3. Interação do usuário:  
     - Alterar o estado do checkbox ativa/desativa os campos relacionados.  
     - Alterações nos campos são salvas no dataset.  

* **Dados Necessários:**  
  - Checkbox para ativar/desativar.  
  - Dias e horário do alerta.  
  - Lista de e-mails.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**  
  - Checkbox deve estar marcado para habilitar os campos de alerta e e-mails.  
  - Campos de alerta e e-mails são limpos automaticamente quando o checkbox é desmarcado.  

* **Filtros Disponíveis:**  
  - Não há filtros explícitos definidos no código.  

* **Mensagens de Erro:**  
  - Não há mensagens de erro explícitas no código.  

* **Valores Padrão dos Campos:**  
  - Não há valores padrão definidos no código.  

* **Validações e Condições dos Campos:**  
  - `EDTcsaEmails`: Deve conter um formato válido de e-mail (não definido no código).  
  - `EDTalertTime`: Deve conter um horário válido.  
  - `EDTalertDays`: Deve conter um número válido de dias.  

---

## 5. Funções Principais:

* **`Create`:**  
  Configura as propriedades iniciais do formulário e define o estilo visual dos componentes.  

* **`ShowData`:**  
  Atualiza o estado dos componentes com base no valor do checkbox.  

* **`SetComponentesState`:**  
  Habilita ou desabilita os campos de alerta e e-mails.  

* **`CHKcheckPreDelivClick`:**  
  Controla a ativação/desativação dos campos relacionados ao checkbox.  

---

## 6. Consumo de Serviços API:

* Não há chamadas a serviços externos definidas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O estado dos campos `EDTalertDays`, `EDTalertTime` e `EDTcsaEmails` depende do valor do checkbox `CHKcheckPreDeliv`.  
* **Condição:** Os campos são habilitados apenas quando o checkbox está marcado.

---

## 8. Dependências:

* **Bibliotecas Externas:**  
  - `kneFRCtrlEditSOA`, `kneFREditSOA`, `kneTypes`, `kneUtils`: Utilizadas para funcionalidades específicas do framework.  
  - `DMskin`: Gerencia estilos visuais dos componentes.  

* **Componentes Customizados:**  
  - `TsLabel`, `TsDBCheckBox`, `TsDBEdit`: Componentes visuais personalizados.  

---

## 9. Listagem de Campos e Validações:

* **CHKcheckPreDeliv** (Checkbox):  
  - Tipo: Booleano.  
  - Valores: `Y` (marcado), `N` (desmarcado).  

* **EDTcsaEmails** (Texto):  
  - Tipo: String.  
  - Validação: Não definida no código.  

* **EDTalertTime** (Tempo):  
  - Tipo: Hora.  
  - Validação: Não definida no código.  

* **EDTalertDays** (Máscara):  
  - Tipo: Número.  
  - Validação: Não definida no código.  

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**  
  Não aplicável.  

* **Diagrama de Sequência:**  
  Não aplicável.  

* **Exemplo de Código:**  
  ```delphi
  CHKcheckPreDeliv.Checked := True; // Ativa os campos relacionados
  EDTalertDays.Text := '3';         // Configura 3 dias de alerta
  EDTalertTime.Text := '08:00';     // Configura o horário do alerta
  EDTcsaEmails.Text := 'email@exemplo.com'; // Adiciona e-mail
  ```

* **HTML Renderizado:**  
  ```html
  <div style="font-family: Verdana; color: #5059883;">
    <label for="alertDays">Alert Days:</label>
    <input type="text" id="alertDays" style="width: 100px;" />
    <br />
    <label for="alertTime">Alert Time:</label>
    <input type="time" id="alertTime" style="width: 100px;" />
    <br />
    <label for="csaEmails">CSA Emails:</label>
    <input type="email" id="csaEmails" style="width: 300px;" />
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial no construtor `Create`.  
* Método `SetComponentesState` para gerenciar o estado dos componentes.  

---

## 12. Conclusão:

O código fornece uma interface funcional para gerenciar informações de entrega prévia. Ele é bem estruturado, mas carece de validações explícitas e mensagens de erro. Sua integração com o banco de dados é eficiente, mas poderia ser aprimorada com validações adicionais.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar alertas de entrega prévia, permitindo configurar dias, horários e e-mails associados. Ele utiliza componentes visuais personalizados e integra-se ao banco de dados para salvar as informações.#### **FRextShipDelConsMkt.pas**

```
unit FRextShipDelConsMkt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  cxGraphics, cxControls, cxContainer, cxEdit, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxImageComboBox, cxDBEdit, kneFRStatusInfo,
  sFrameAdapter, sBitBtn, sPanel, sLabel, sBevel, sCheckBox, sDBCheckBox,
  sDBEdit, cxSpinEdit, cxTimeEdit;

type
  TFRAMEextShipDelConsMkt = class(TFRAMEBaseCtrlEditSOA)
    LBLname: TsLabel;
    Label1: TsLabel;
    Label2: TsLabel;
    CHKcheckPreDeliv: TsDBCheckBox;
    EDTcsaEmails: TsDBEdit;
    EDTalertTime: TcxDBTimeEdit;
    EDTalertDays: TcxDBMaskEdit;
    procedure CHKcheckPreDelivClick(Sender: TObject);
    procedure CDStableAfterEdit(DataSet: TDataSet);
  private
    procedure SetComponentesState(pv_State: Boolean);
    { Private declarations }

  protected
    function m_Validate: Boolean;  override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ShowData; override;
  end;

var
  FRAMEextShipDelConsMkt: TFRAMEextShipDelConsMkt;

implementation

uses kneFREditSOA, kneTypes, kneUtils, Global, DMskin;

{$R *.dfm}

{ TFRAMEextShipDelConsMkt }

constructor TFRAMEextShipDelConsMkt.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'marketCode=consMkt';
  DataPacketName  := 'ShipDelConsMkt';
  PropertyName    := 'shipDelConsMkt';
  FrameType       := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  EDTalertTime.Style.StyleController := DMODskin.cxEditStyles1;

end;

procedure TFRAMEextShipDelConsMkt.ShowData;
begin
  inherited;

  SetComponentesState(CHKcheckPreDeliv.Checked);
end;

procedure TFRAMEextShipDelConsMkt.SetComponentesState(pv_State: Boolean);
begin
  TkneControls.SetControlState(EDTalertDays, pv_State);
  TkneControls.SetControlState(EDTalertTime, pv_State);
  TkneControls.SetControlState(EDTcsaEmails, pv_State);
end;

procedure TFRAMEextShipDelConsMkt.CHKcheckPreDelivClick(Sender: TObject);
var
  lv_EnableComps : Boolean;
begin
  inherited;
    // activar/desactivar componentes relacionados
  lv_EnableComps := CHKcheckPreDeliv.Checked;

  SetComponentesState(lv_EnableComps);

  if CDSEdition(CDStable)then
  begin

    if not lv_EnableComps then
    begin
      CDStable.FieldByName('alertDays').Clear;
      CDStable.FieldByName('alertTime').Clear;
      CDStable.FieldByName('csaEmails').Clear;

```

#### **FRextShipDelConsMkt.dfm**

```
inherited FRAMEextShipDelConsMkt: TFRAMEextShipDelConsMkt
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBLname: TsLabel [0]
    Left = 42
    Top = 41
    Width = 65
    Height = 13
    Caption = 'Alert Da&ys:'
    FocusControl = EDTalertDays
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label1: TsLabel [1]
    Left = 42
    Top = 67
    Width = 64
    Height = 13
    Caption = 'Alert T&ime:'
    FocusControl = EDTalertTime
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label2: TsLabel [2]
    Left = 42
    Top = 93
    Width = 71
    Height = 13
    Caption = 'CSA Emai&ls:'
    FocusControl = EDTcsaEmails
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 215
    Width = 721
    TabOrder = 4
    Visible = False
    inherited PNLeditActions: TsPanel
      Width = 577
    end
  end
  object CHKcheckPreDeliv: TsDBCheckBox [4]
    Left = 8
    Top = 11
    Width = 163
    Height = 19
    Caption = '&PreDelivery Information'
    TabOrder = 0
    OnClick = CHKcheckPreDelivClick
    SkinData.SkinSection = 'CHECKBOX'
    ImgChecked = 0
    ImgUnchecked = 0
    DataField = 'checkPreDeliv'
    DataSource = DStable
    ValueChecked = 'Y'
    ValueUnchecked = 'N'
  end
  object EDTcsaEmails: TsDBEdit [5]
    Left = 116
    Top = 88
    Width = 605
    Height = 21
    AutoSize = False
    BevelWidth = 0
    Color = clWhite
    DataField = 'csaEmails'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
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
  object EDTalertTime: TcxDBTimeEdit [6]
    Left = 116
```
<!-- tabs:end -->

