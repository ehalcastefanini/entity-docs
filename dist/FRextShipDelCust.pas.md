<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um formulário para gerenciar configurações relacionadas à entrega de clientes (ShipDelCustomer). Ele permite que o usuário configure alertas de entrega, como dias de alerta, horário de alerta e e-mails associados. Além disso, o formulário possui lógica para ativar ou desativar campos com base em uma checkbox de pré-entrega.

* **Tecnologias Utilizadas:**  
  - Delphi (VCL Framework).
  - Componentes visuais como `TsLabel`, `TsDBCheckBox`, `TcxDBTimeEdit`, `TcxDBMaskEdit`, e `TsDBEdit`.
  - Manipulação de banco de dados com `TDataSet` e `TClientDataSet`.

* **Tipo de Formulário:**  
  - **Formulário:**  
    - **Elementos do Formulário e Tipos:**  
      - `CHKcheckPreDeliv` (Checkbox): Ativa ou desativa os campos relacionados a alertas de entrega.  
      - `EDTcsaEmails` (Campo de texto): Campo para inserir e-mails.  
      - `EDTalertTime` (Campo de hora): Campo para configurar o horário do alerta.  
      - `EDTalertDays` (Campo de máscara): Campo para configurar os dias de alerta.  
    - **Ações do Formulário e Efeitos:**  
      - Clique na checkbox ativa/desativa os campos relacionados.  
      - Alterações nos campos são validadas e salvas no banco de dados.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**  
  - Ativar/desativar campos relacionados a alertas de entrega.  
  - Configurar dias, horário e e-mails para alertas.  
  - Validar e salvar os dados no banco de dados.

* **Componentes Principais:**  
  - `CHKcheckPreDeliv`: Controla a ativação dos campos.  
  - `SetComponentesState`: Método que altera o estado dos campos com base na checkbox.  
  - `m_Validate`: Método para validação dos dados antes de salvar.  
  - `ShowData`: Método para exibir os dados no formulário.

* **Tradução para Pseudo-código:**  
  - Evento `OnClick` da checkbox:  
    ```pseudo
    se checkbox marcada então
        ativar campos relacionados
    senão
        desativar campos relacionados e limpar valores
    ```
  - Evento `AfterEdit` do dataset:  
    ```pseudo
    se dataset for editado então
        validar e salvar alterações
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**  
  1. Inicialização do formulário (`Create`): Configura propriedades e estilos.  
  2. Exibição de dados (`ShowData`): Carrega os dados e ajusta o estado dos componentes com base na checkbox.  
  3. Interação do usuário:  
     - Clique na checkbox ativa/desativa os campos.  
     - Alterações nos campos são validadas e salvas.  

* **Dados Necessários:**  
  - Dias de alerta (`EDTalertDays`).  
  - Horário de alerta (`EDTalertTime`).  
  - E-mails para alerta (`EDTcsaEmails`).  

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**  
  - Checkbox `CHKcheckPreDeliv`: Deve estar marcada para ativar os campos de alerta.  
  - Campos de alerta: Devem ser preenchidos apenas se a checkbox estiver marcada.

* **Filtros Disponíveis:**  
  - Não aplicável (não há filtros explícitos no código).

* **Mensagens de Erro:**  
  - Não definidas explicitamente no código.

* **Valores Padrão dos Campos:**  
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**  
  - `EDTalertDays`: Deve aceitar apenas números (não definido explicitamente).  
  - `EDTalertTime`: Deve aceitar apenas valores de hora (não definido explicitamente).  
  - `EDTcsaEmails`: Deve aceitar apenas texto (não definido explicitamente).

---

## 5. Funções Principais:

* **`Create`:**  
  Configura propriedades do formulário e estilos dos componentes.  

* **`ShowData`:**  
  Exibe os dados no formulário e ajusta o estado dos componentes.  

* **`SetComponentesState`:**  
  Ativa ou desativa os campos de alerta com base no estado da checkbox.  

* **`CHKcheckPreDelivClick`:**  
  Evento que controla a ativação/desativação dos campos e limpa os valores se necessário.  

---

## 6. Consumo de Serviços API:

* Não há chamadas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* **Campo Condicional:**  
  - Os campos `EDTalertDays`, `EDTalertTime` e `EDTcsaEmails` só são visíveis/ativos se a checkbox `CHKcheckPreDeliv` estiver marcada.  

---

## 8. Dependências:

* **Bibliotecas Externas:**  
  - `TsLabel`, `TsDBCheckBox`, `TcxDBTimeEdit`, `TcxDBMaskEdit`, `TsDBEdit` (componentes visuais).  

* **Componentes Customizados:**  
  - `TkneControls`: Usado para alterar o estado dos componentes.  

---

## 9. Listagem de Campos e Validações:

* **Campos:**  
  - `CHKcheckPreDeliv` (Checkbox, obrigatório).  
  - `EDTalertDays` (Texto, opcional).  
  - `EDTalertTime` (Hora, opcional).  
  - `EDTcsaEmails` (Texto, opcional).  

* **Mapeamento de Valores e Colunas do Banco de Dados:**  
  - `EDTalertDays` → `alertDays`.  
  - `EDTalertTime` → `alertTime`.  
  - `EDTcsaEmails` → `csaEmails`.  

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```delphi
  FRAMEextShipDelCust := TFRAMEextShipDelCust.Create(Self);
  FRAMEextShipDelCust.ShowData;
  ```

* **HTML Representando o Formulário:**  
  ```html
  <div style="font-family: Verdana; color: #4D4D4D;">
    <label for="alertDays">Alert Days:</label>
    <input type="text" id="alertDays" style="margin-bottom: 10px;"><br>
    <label for="alertTime">Alert Time:</label>
    <input type="time" id="alertTime" style="margin-bottom: 10px;"><br>
    <label for="csaEmails">CSA Emails:</label>
    <input type="email" id="csaEmails" style="margin-bottom: 10px;"><br>
    <input type="checkbox" id="checkPreDeliv"> Enable Alerts
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades no construtor (`Create`).  
* Lógica de ativação/desativação de campos no método `SetComponentesState`.  

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar alertas de entrega de clientes. Ele é bem estruturado, mas carece de validações explícitas e mensagens de erro. Além disso, não há suporte para filtros ou valores padrão.

---

## 13. Resumo Curto:

O código implementa um formulário para configurar alertas de entrega de clientes, permitindo ativar/desativar campos com base em uma checkbox. Ele é parte de um sistema maior de gerenciamento de entregas.#### **FRextShipDelCust.pas**

```
unit FRextShipDelCust;

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
  TFRAMEextShipDelCust = class(TFRAMEBaseCtrlEditSOA)
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
  FRAMEextShipDelCust: TFRAMEextShipDelCust;

implementation

uses kneFREditSOA, kneTypes, kneUtils, Global, DMskin;

{$R *.dfm}

{ TFRAMEextShipDelCust }

constructor TFRAMEextShipDelCust.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=cust';
  DataPacketName  := 'ShipDelCustomer';
  PropertyName    := 'shipDelCustomer';
  FrameType       := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  EDTalertTime.Style.StyleController := DMODskin.cxEditStyles1;

end;

procedure TFRAMEextShipDelCust.ShowData;
begin
  inherited;

  SetComponentesState(CHKcheckPreDeliv.Checked);
end;

procedure TFRAMEextShipDelCust.SetComponentesState(pv_State: Boolean);
begin
  TkneControls.SetControlState(EDTalertDays, pv_State);
  TkneControls.SetControlState(EDTalertTime, pv_State);
  TkneControls.SetControlState(EDTcsaEmails, pv_State);
end;

procedure TFRAMEextShipDelCust.CHKcheckPreDelivClick(Sender: TObject);
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

#### **FRextShipDelCust.dfm**

```
inherited FRAMEextShipDelCust: TFRAMEextShipDelCust
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
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
```
<!-- tabs:end -->

