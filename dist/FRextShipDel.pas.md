<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um formulário para gerenciar informações de pré-entrega (PreDelivery Information) relacionadas a alertas de tempo, dias e e-mails associados. Ele permite que o usuário configure e visualize essas informações de forma dinâmica, ativando ou desativando os campos relacionados com base em uma checkbox.

* **Tecnologias Utilizadas:**  
  - Delphi (VCL Framework).
  - Componentes visuais como `TsLabel`, `TsDBCheckBox`, `TsDBEdit`, `TcxDBTimeEdit`, e `TcxDBMaskEdit`.
  - Manipulação de banco de dados com `TDataSet` e `TClientDataSet`.

* **Tipo de Formulário:**  
  - **Formulário:**  
    - **Elementos do Formulário e Tipos:**  
      - `CHKcheckPreDeliv` (Checkbox): Ativa/desativa os campos relacionados à pré-entrega.  
      - `EDTcsaEmails` (Campo de texto): Campo para inserir e-mails.  
      - `EDTalertTime` (Campo de hora): Campo para configurar o horário do alerta.  
      - `EDTalertDays` (Campo de máscara): Campo para configurar os dias do alerta.  
    - **Ações do Formulário e Efeitos:**  
      - Alterar o estado da checkbox ativa ou desativa os campos relacionados.  
      - Limpar os campos quando a checkbox é desmarcada.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**  
  - Ativar/desativar os campos de alerta com base na checkbox `CHKcheckPreDeliv`.
  - Preencher os campos de e-mail, horário e dias de alerta.
  - Limpar os campos automaticamente ao desativar a checkbox.

* **Componentes Principais:**  
  - `CHKcheckPreDeliv`: Controla a ativação dos campos relacionados.  
  - `EDTcsaEmails`, `EDTalertTime`, `EDTalertDays`: Campos de entrada de dados.  
  - `SetComponentesState`: Método que altera o estado dos componentes com base na checkbox.

* **Pseudo-código dos Eventos e Ações:**  
  - Evento `OnClick` da checkbox:  
    ```pseudo
    se checkbox marcada então
        habilitar campos relacionados
    senão
        desabilitar campos relacionados
        limpar valores dos campos
    ```
  - Evento `AfterEdit` do dataset:  
    ```pseudo
    se o dataset for editado então
        validar e salvar alterações
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**  
  1. Inicialização do formulário (`Create`): Configura propriedades iniciais e estilos.  
  2. Exibição de dados (`ShowData`): Atualiza o estado dos componentes com base na checkbox.  
  3. Interação do usuário:  
     - Marcar/desmarcar a checkbox ativa/desativa os campos e limpa os valores, se necessário.  
     - Preencher os campos de alerta e salvar as alterações.

* **Dados Necessários:**  
  - Checkbox `CHKcheckPreDeliv`: Define se os campos relacionados serão ativados.  
  - Campos de entrada:  
    - Dias de alerta (`EDTalertDays`).  
    - Horário de alerta (`EDTalertTime`).  
    - E-mails (`EDTcsaEmails`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**  
  - A checkbox deve ser marcada para habilitar os campos de entrada.  
  - Os campos são limpos automaticamente ao desmarcar a checkbox.

* **Filtros Disponíveis:**  
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**  
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**  
  - Não há valores padrão definidos no código.

* **Validações e Condições dos Campos:**  
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`Create` (Construtor):**  
  Configura propriedades iniciais do formulário, como campos de chave, nome do pacote de dados e tipo de frame.

* **`ShowData`:**  
  Atualiza o estado dos componentes com base na checkbox.

* **`SetComponentesState`:**  
  Habilita ou desabilita os campos relacionados à pré-entrega.

* **`CHKcheckPreDelivClick`:**  
  Controla a ativação/desativação dos campos e limpa os valores, se necessário.

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos definidas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O estado dos campos `EDTalertDays`, `EDTalertTime` e `EDTcsaEmails` depende da checkbox `CHKcheckPreDeliv`.  
* **Condição:** Os campos são habilitados apenas quando a checkbox está marcada.

---

## 8. Dependências:

* **Bibliotecas Externas:**  
  - `kneFRCtrlEditSOA`, `InvokeRegistry`, `SOAPHTTPClient`: Utilizadas para manipulação de dados e integração SOAP.  
  - `cxGraphics`, `cxControls`, `cxEdit`: Componentes visuais avançados.  

* **Componentes Personalizados:**  
  - `TsLabel`, `TsDBCheckBox`, `TsDBEdit`: Componentes visuais personalizados para estilização.

---

## 9. Listagem de Campos e Validações:

* **Campos do Formulário:**  
  - `CHKcheckPreDeliv` (Checkbox, obrigatório): Controla a ativação dos campos.  
  - `EDTcsaEmails` (Texto, opcional): Campo para inserir e-mails.  
  - `EDTalertTime` (Hora, opcional): Campo para configurar o horário do alerta.  
  - `EDTalertDays` (Máscara, opcional): Campo para configurar os dias do alerta.  

* **Mapeamento de Valores e Colunas do Banco de Dados:**  
  - `CHKcheckPreDeliv` → `checkPreDeliv`.  
  - `EDTcsaEmails` → `csaEmails`.  
  - `EDTalertTime` → `alertTime`.  
  - `EDTalertDays` → `alertDays`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**  
  ```plaintext
  [Início] --> [Inicializar Formulário] --> [Exibir Dados] --> [Interação do Usuário]
  --> [Marcar Checkbox] --> [Habilitar Campos] --> [Preencher Dados] --> [Salvar]
  ```

* **Diagrama de Sequência:**  
  ```plaintext
  Usuário --> Formulário: Marca checkbox
  Formulário --> Banco de Dados: Atualiza estado dos campos
  Usuário --> Formulário: Preenche campos
  Formulário --> Banco de Dados: Salva alterações
  ```

* **Código HTML Representando o Formulário:**  
  ```html
  <form style="font-family: Verdana; width: 500px;">
    <label for="alertDays">Alert Days:</label>
    <input type="text" id="alertDays" style="width: 100%;"><br>
    <label for="alertTime">Alert Time:</label>
    <input type="time" id="alertTime" style="width: 100%;"><br>
    <label for="csaEmails">CSA Emails:</label>
    <input type="email" id="csaEmails" style="width: 100%;"><br>
    <input type="checkbox" id="preDelivery"> PreDelivery Information
  </form>
  ```

---

## 11. Comentários Importantes no Código:

* O método `SetComponentesState` é essencial para controlar o estado dos campos com base na checkbox.  
* O construtor `Create` configura propriedades importantes para o funcionamento do formulário.

---

## 12. Conclusão:

O código implementa um formulário funcional e dinâmico para gerenciar informações de pré-entrega. Ele é bem estruturado, mas carece de validações explícitas e mensagens de erro. Sua integração com banco de dados é eficiente, mas poderia ser aprimorada com validações adicionais.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar informações de pré-entrega, permitindo ativar/desativar campos dinamicamente com base em uma checkbox. Ele é integrado ao banco de dados e utiliza componentes personalizados para estilização e funcionalidade.#### **FRextShipDel.pas**

```
unit FRextShipDel;

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
  TFRAMEextShipDel = class(TFRAMEBaseCtrlEditSOA)
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
  FRAMEextShipDel: TFRAMEextShipDel;

implementation

uses kneFREditSOA, kneTypes, kneUtils, Global, DMskin;

{$R *.dfm}

{ TFRAMEextShipDel }

constructor TFRAMEextShipDel.Create(AOwner: TComponent);
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

procedure TFRAMEextShipDel.ShowData;
begin
  inherited;

  SetComponentesState(CHKcheckPreDeliv.Checked);
end;

procedure TFRAMEextShipDel.SetComponentesState(pv_State: Boolean);
begin
  TkneControls.SetControlState(EDTalertDays, pv_State);
  TkneControls.SetControlState(EDTalertTime, pv_State);
  TkneControls.SetControlState(EDTcsaEmails, pv_State);
end;

procedure TFRAMEextShipDel.CHKcheckPreDelivClick(Sender: TObject);
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

#### **FRextShipDel.dfm**

```
inherited FRAMEextShipDel: TFRAMEextShipDel
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
    Width = 501
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
    Width = 385
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

