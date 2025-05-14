<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um componente de interface gráfica para gerenciar configurações relacionadas a alertas de entrega de remessas. Ele permite que o usuário configure dias de alerta, horários de alerta e e-mails de notificação, com base em uma opção de ativação/desativação.

* **Tecnologias Utilizadas:**  
  - Delphi (Object Pascal) para desenvolvimento da interface e lógica.
  - Componentes visuais como `TsLabel`, `TsDBCheckBox`, `TsDBEdit`, `TcxDBTimeEdit`, e `TcxDBMaskEdit`.
  - Integração com banco de dados via `TDataSet` e `TClientDataSet`.

* **Forma:**  
  Este código implementa um formulário com os seguintes elementos:
  - **Elementos do Formulário:**
    - `CHKcheckPreDeliv` (Checkbox): Ativa/desativa os alertas.
    - `EDTalertDays` (Campo de texto): Configura os dias de alerta.
    - `EDTalertTime` (Campo de horário): Configura o horário do alerta.
    - `EDTcsaEmails` (Campo de texto): Configura os e-mails de notificação.
  - **Ações do Formulário:**
    - Alterar o estado dos campos com base na seleção do checkbox.
    - Limpar os campos quando o checkbox é desmarcado.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Ativar/desativar alertas de entrega.
  - Configurar dias e horários de alerta.
  - Configurar e-mails de notificação.

* **Componentes Principais:**
  - `CHKcheckPreDeliv`: Controla a ativação dos alertas.
  - `SetComponentesState`: Gerencia o estado (habilitado/desabilitado) dos campos.
  - `ShowData`: Atualiza os dados exibidos no formulário.

* **Pseudocódigo das Ações e Eventos:**
  - Evento `OnClick` do checkbox:
    ```pseudo
    se checkbox marcado então
        habilitar campos de configuração
    senão
        desabilitar campos de configuração
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
  1. Inicialização do componente (`Create`):
     - Configura propriedades como `MasterKeyFields`, `DataPacketName`, e `FrameType`.
     - Define estilos visuais para os campos.
  2. Exibição de dados (`ShowData`):
     - Atualiza o estado dos componentes com base no valor do checkbox.
  3. Interação do usuário:
     - O usuário marca/desmarca o checkbox, o que habilita/desabilita os campos e limpa os valores, se necessário.

* **Dados Necessários:**
  - Dias de alerta.
  - Horário de alerta.
  - E-mails de notificação.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Checkbox `CHKcheckPreDeliv`:
    - **Pré-condição:** Nenhuma.
    - **Ação:** Habilita/desabilita os campos de configuração.
  - Campos de configuração:
    - **Pré-condição:** Checkbox deve estar marcado.

* **Filtros Disponíveis:**
  - Não aplicável (não há filtros explícitos no código).

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas no código.

* **Valores Padrão dos Campos:**
  - Não definidos no código.

* **Validações e Condições dos Campos:**
  - Não há validações explícitas no código.

---

## 5. Funções Principais:

* **`Create`:**  
  Configura as propriedades iniciais do componente e define estilos visuais.

* **`ShowData`:**  
  Atualiza os dados exibidos no formulário e ajusta o estado dos componentes.

* **`SetComponentesState`:**  
  Habilita ou desabilita os campos de configuração com base no estado do checkbox.

* **`CHKcheckPreDelivClick`:**  
  Gerencia a interação do usuário com o checkbox, ajustando o estado dos campos e limpando valores, se necessário.

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O estado dos campos `EDTalertDays`, `EDTalertTime` e `EDTcsaEmails` depende do valor do checkbox `CHKcheckPreDeliv`.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `TsLabel`, `TsDBCheckBox`, `TsDBEdit`, `TcxDBTimeEdit`, `TcxDBMaskEdit`: Componentes visuais.
  - `TDataSet`, `TClientDataSet`: Integração com banco de dados.

* **Componentes Customizados:**
  - `TkneControls`: Gerencia o estado dos controles.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `CHKcheckPreDeliv` (checkbox, obrigatório): Ativa/desativa os alertas.
  - `EDTalertDays` (string, opcional): Dias de alerta.
  - `EDTalertTime` (horário, opcional): Horário de alerta.
  - `EDTcsaEmails` (string, opcional): E-mails de notificação.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `alertDays` → `EDTalertDays`.
  - `alertTime` → `EDTalertTime`.
  - `csaEmails` → `EDTcsaEmails`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```pascal
  FRAMEextShipDelCons := TFRAMEextShipDelCons.Create(Self);
  FRAMEextShipDelCons.ShowData;
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

* **`SetComponentesState`:**  
  Gerencia o estado dos campos de configuração com base no checkbox.

* **`CHKcheckPreDelivClick`:**  
  Limpa os valores dos campos quando o checkbox é desmarcado.

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar alertas de entrega, com uma interface simples e lógica clara. No entanto, faltam validações explícitas e mensagens de erro, o que pode limitar sua robustez.

---

## 13. Resumo Curto:

O código implementa um formulário para configurar alertas de entrega, permitindo ativar/desativar alertas, definir dias/horários e e-mails de notificação. Ele utiliza componentes visuais e integração com banco de dados para gerenciar os dados.#### **FRextShipDelCons.pas**

```
unit FRextShipDelCons;

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
  TFRAMEextShipDelCons = class(TFRAMEBaseCtrlEditSOA)
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
  FRAMEextShipDelCons: TFRAMEextShipDelCons;

implementation

uses kneFREditSOA, kneTypes, kneUtils, Global, DMskin;

{$R *.dfm}

{ TFRAMEextShipDelCons }

constructor TFRAMEextShipDelCons.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode=cons';
  DataPacketName  := 'ShipDelConsignee';
  PropertyName    := 'shipDelConsignee';
  FrameType       := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  EDTalertTime.Style.StyleController := DMODskin.cxEditStyles1;

end;

procedure TFRAMEextShipDelCons.ShowData;
begin
  inherited;

  SetComponentesState(CHKcheckPreDeliv.Checked);
end;

procedure TFRAMEextShipDelCons.SetComponentesState(pv_State: Boolean);
begin
  TkneControls.SetControlState(EDTalertDays, pv_State);
  TkneControls.SetControlState(EDTalertTime, pv_State);
  TkneControls.SetControlState(EDTcsaEmails, pv_State);
end;

procedure TFRAMEextShipDelCons.CHKcheckPreDelivClick(Sender: TObject);
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

#### **FRextShipDelCons.dfm**

```
inherited FRAMEextShipDelCons: TFRAMEextShipDelCons
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

